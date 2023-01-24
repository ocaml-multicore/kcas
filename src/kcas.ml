(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

module type Backoff = Backoff.S

module Backoff = Backoff.M

module Id = struct
  let id = Atomic.make 1
  let get_unique () = Atomic.fetch_and_add id 1
end

type 'a state = WORD of 'a | RDCSS_DESC of 'a rdcss_t | CASN_DESC of casn_t
and 'a ref = { content : 'a state Atomic.t; id : int }
and t = CAS : 'a ref * 'a state * 'a state -> t
and status = UNDECIDED | FAILED | SUCCEEDED

and 'a rdcss_t = {
  a1 : status ref; (* control value *)
  o1 : status state; (* expected control value *)
  a2 : 'a ref; (* data value *)
  o2 : 'a state; (* old data *)
  n2 : 'a state; (* new data *)
  id_rdcss : int;
}

and casn_t = { st : status ref; c_l : t list; id_casn : int }
and 'a cas_result = Aborted | Failed | Success of 'a

let ref a = { content = Atomic.make (WORD a); id = Id.get_unique () }
let equal r1 r2 = Obj.repr r1 == Obj.repr r2
let is_on_ref (CAS (r1, _, _)) r2 = equal r1 r2
let mk_cas a old_value new_value = CAS (a, WORD old_value, WORD new_value)

let mk_rdcss a1 o1 a2 o2 n2 =
  { a1; o1; a2; o2; n2; id_rdcss = Id.get_unique () }

let mk_casn st c_l = { st; c_l; id_casn = Id.get_unique () }

let st_eq s s' =
  match (s, s') with
  | WORD x, WORD x' -> x == x'
  | RDCSS_DESC r, RDCSS_DESC r' -> r.id_rdcss == r'.id_rdcss
  | CASN_DESC c, CASN_DESC c' -> c.id_casn == c'.id_casn
  | _ -> false

let commit (CAS (r, expect, update)) =
  let curr_value = Atomic.get r.content in
  st_eq curr_value expect && Atomic.compare_and_set r.content curr_value update

let cas r e u = commit (mk_cas r e u)
let set r n = Atomic.set r.content (WORD n)
let get_id r = r.id

let rec rdcss rd =
  if commit (CAS (rd.a2, rd.o2, RDCSS_DESC rd)) then (
    ignore @@ complete rd;
    rd.o2)
  else
    let curr_data = Atomic.get rd.a2.content in
    match curr_data with
    | RDCSS_DESC rd' ->
        ignore @@ complete rd';
        rdcss rd
    | WORD _ | CASN_DESC _ ->
        if st_eq curr_data rd.o2 then rdcss rd else curr_data

and complete rd =
  if st_eq (Atomic.get rd.a1.content) rd.o1 then
    commit (CAS (rd.a2, RDCSS_DESC rd, rd.n2))
  else commit (CAS (rd.a2, RDCSS_DESC rd, rd.o2))

let rec rdcss_read a =
  let r = Atomic.get a in
  match r with
  | RDCSS_DESC rd ->
      ignore @@ complete rd;
      rdcss_read a
  | _ -> r

let rec casn_proceed c =
  let rec phase1 curr_cas_list curr_status out =
    match curr_cas_list with
    | CAS (atomic, old_value, new_value) :: curr_c_t_tail
      when curr_status = SUCCEEDED -> (
        let s =
          rdcss (mk_rdcss c.st (WORD UNDECIDED) atomic old_value (CASN_DESC c))
        in
        match s with
        | CASN_DESC c' ->
            if c.id_casn != c'.id_casn then (
              ignore @@ casn_proceed c';
              phase1 curr_cas_list curr_status out)
            else
              phase1 curr_c_t_tail curr_status
                (CAS (atomic, old_value, new_value) :: out)
        | RDCSS_DESC _ -> assert false
        | WORD _ ->
            if st_eq s old_value then
              phase1 curr_c_t_tail curr_status
                (CAS (atomic, old_value, new_value) :: out)
            else
              phase1 curr_c_t_tail FAILED
                (CAS (atomic, old_value, new_value) :: out))
    | _ ->
        ignore @@ commit (CAS (c.st, WORD UNDECIDED, WORD curr_status));
        out
  in
  let rec phase2 curr_c_l succ =
    match curr_c_l with
    | CAS (a, o, n) :: curr_c_l_tail ->
        let value_to_commit =
          match Atomic.get succ with
          | WORD SUCCEEDED -> n
          | WORD FAILED -> o
          | _ -> assert false
        in
        ignore @@ commit (CAS (a, CASN_DESC c, value_to_commit));
        phase2 curr_c_l_tail succ
    | _ -> Atomic.get succ = WORD SUCCEEDED
  in
  match Atomic.get c.st.content with
  | WORD UNDECIDED -> phase2 (phase1 c.c_l SUCCEEDED []) c.st.content
  | _ -> phase2 c.c_l c.st.content

let rec get a =
  let r = rdcss_read a.content in
  match r with
  | CASN_DESC c ->
      ignore @@ casn_proceed c;
      get a
  | WORD out -> out
  | _ -> assert false

let kCAS ?(presort = true) cas_list =
  match cas_list with
  | [] -> true
  | [ (CAS (a, _, _) as c) ] ->
      ignore @@ get a;
      commit c
  | _ ->
      let cas_list =
        if presort then (
          (* ensure global total order of locations (see section 5 in kCAS paper) *)
          let sorted =
            List.sort
              (fun (CAS (cas_a, _, _)) (CAS (cas_b, _, _)) ->
                Int.compare (get_id cas_a) (get_id cas_b))
              cas_list
          in
          (* check for overlapping locations *)
          List.fold_left
            (fun previous_id (CAS (ref, _, _)) ->
              let current_id = get_id ref in
              if current_id = previous_id then failwith "kcas: location overlap";
              current_id)
            0 sorted
          |> ignore;
          sorted)
        else cas_list
      in

      (* proceed with casn *)
      let casn = mk_casn (ref UNDECIDED) cas_list in
      casn_proceed casn

let try_map r f =
  let c = get r in
  match f c with
  | None -> Aborted
  | Some v -> if kCAS [ mk_cas r c v ] then Success c else Failed

let map r f =
  let b = Backoff.create () in
  let rec loop () =
    match try_map r f with
    | Failed ->
        Backoff.once b;
        loop ()
    | out -> out
  in
  loop ()

let incr r = ignore @@ map r (fun i -> Some (i + 1))
let decr r = ignore @@ map r (fun i -> Some (i - 1))

module type W1 = sig
  type 'a ref

  val ref : 'a -> 'a ref
  val get : 'a ref -> 'a
  val set : 'a ref -> 'a -> unit
  val cas : 'a ref -> 'a -> 'a -> bool
  val try_map : 'a ref -> ('a -> 'a option) -> 'a cas_result
  val map : 'a ref -> ('a -> 'a option) -> 'a cas_result
  val incr : int ref -> unit
  val decr : int ref -> unit
end

module W1 : W1 = struct
  type 'a ref = 'a Atomic.t

  let ref = Atomic.make
  let get = Atomic.get
  let set r n = Atomic.set r n
  let cas = Atomic.compare_and_set

  let try_map r f =
    let s = get r in
    match f s with
    | None -> Aborted
    | Some v -> if cas r s v then Success s else Failed

  let map r f =
    let b = Backoff.create () in
    let rec loop () =
      match try_map r f with
      | Failed ->
          Backoff.once b;
          loop ()
      | v -> v
    in
    loop ()

  let incr r = ignore @@ map r (fun x -> Some (x + 1))
  let decr r = ignore @@ map r (fun x -> Some (x - 1))
end
