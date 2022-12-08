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

type 'a state =
  | WORD : 'a -> 'a state
  | RDCSS_DESC : 'a rdcss_t -> 'a state
  | CASN_DESC : casn_t -> 'a state

and 'a ref = { content : 'a state Atomic.t; id : int }
and t = CAS : 'a ref * 'a state * 'a state -> t
and status = UNDECIDED | FAILED | SUCCEEDED

and 'a rdcss_t = {
  a1 : status ref;
  o1 : status state;
  a2 : 'a ref;
  o2 : 'a state;
  n2 : 'a state;
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
  let s = Atomic.get r.content in
  st_eq s expect && Atomic.compare_and_set r.content s update

let cas r e u = commit (mk_cas r e u)
let set r n = Atomic.set r.content (WORD n)
let get_id r = r.id

let rec rdcss rd =
  if commit (CAS (rd.a2, rd.o2, RDCSS_DESC rd)) then (
    ignore @@ complete rd;
    rd.o2)
  else
    let r = Atomic.get rd.a2.content in
    match r with
    | RDCSS_DESC rd' ->
        ignore @@ complete rd';
        rdcss rd
    | _ -> if st_eq r rd.o2 then rdcss rd else r

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
  let rec phase1 curr_c_l curr_st out =
    match curr_c_l with
    | CAS (a, o, n) :: curr_c_t_tail when curr_st = SUCCEEDED -> (
        let s = rdcss (mk_rdcss c.st (WORD UNDECIDED) a o (CASN_DESC c)) in
        match s with
        | CASN_DESC c' ->
            if c.id_casn != c'.id_casn then (
              ignore @@ casn_proceed c';
              phase1 curr_c_l curr_st out)
            else phase1 curr_c_t_tail curr_st (CAS (a, o, n) :: out)
        | _ ->
            if st_eq s o then phase1 curr_c_t_tail curr_st (CAS (a, o, n) :: out)
            else phase1 curr_c_t_tail FAILED (CAS (a, o, n) :: out))
    | _ ->
        ignore @@ commit (CAS (c.st, WORD UNDECIDED, WORD curr_st));
        out
  in
  let rec phase2 curr_c_l succ =
    match curr_c_l with
    | CAS (a, o, n) :: curr_c_l_tail -> (
        match Atomic.get succ with
        | WORD SUCCEEDED ->
            ignore @@ commit (CAS (a, CASN_DESC c, n));
            phase2 curr_c_l_tail succ
        | _ ->
            ignore @@ commit (CAS (a, CASN_DESC c, o));
            phase2 curr_c_l_tail succ)
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

let kCAS cas_list =
  match cas_list with
  | [] -> true
  | [ (CAS (a, _, _) as c) ] ->
      ignore @@ get a;
      commit c
  | _ -> casn_proceed (mk_casn (ref UNDECIDED) cas_list)

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
