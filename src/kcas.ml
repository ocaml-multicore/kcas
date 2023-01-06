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

type 'a ref = { state : 'a state Atomic.t; id : int }
and 'a state = { mutable before : 'a; mutable after : 'a; mutable casn : casn }
and cas = CAS : 'a state Atomic.t * 'a state -> cas
and casn = status Atomic.t
and status = [ `Undetermined of cas list | `After | `Before ]

type t = T : 'a ref * 'a * 'a -> t
type 'a cas_result = Aborted | Failed | Success of 'a

let casn_after = Atomic.make `After
let casn_before = Atomic.make `Before

let ref after =
  {
    state = Atomic.make { before = after; after; casn = casn_after };
    id = Id.get_unique ();
  }

let equal r1 r2 = Obj.repr r1 == Obj.repr r2
let is_on_ref (T (r1, _, _)) r2 = equal r1 r2
let mk_cas a old_value new_value = T (a, old_value, new_value)
let get_id r = r.id

let set atom value =
  Atomic.set atom.state { before = value; after = value; casn = casn_after }

let rec release_after = function
  | [] -> true
  | CAS (_, state) :: cass ->
      state.casn <- casn_after;
      state.before <- state.after;
      release_after cass

let rec release_before = function
  | [] -> false
  | CAS (_, state) :: cass ->
      state.casn <- casn_before;
      state.after <- state.before;
      release_before cass

(* Note: The writes to `state.casn <- ...` above could be removed to reduce time
   at the cost of increasing space usage (by a constant factor). *)

let release cass = function
  | `After -> release_after cass
  | `Before -> release_before cass

let finish casn (`Undetermined cass as undetermined)
    (status : [ `Before | `After ]) =
  if Atomic.compare_and_set casn (undetermined :> status) (status :> status)
  then release cass status
  else Atomic.get casn == `After

let rec determine casn undetermined = function
  | CAS (atom_state, state) :: cass_1 as cass_0 ->
      let state' = Atomic.get atom_state in
      if state == state' then determine casn undetermined cass_1
      else
        let casn' = state'.casn in
        let is_after =
          match Atomic.get casn' with
          | `After -> true
          | `Before -> false
          | `Undetermined cass' as undetermined' ->
              determine casn' undetermined' cass'
        in
        let value = if is_after then state'.after else state'.before in
        if value != state.before then finish casn undetermined `Before
        else
          let status = Atomic.get casn in
          if status != (undetermined :> status) then status == `After
          else if Atomic.compare_and_set atom_state state' state then
            determine casn undetermined cass_1
          else determine casn undetermined cass_0
  | [] -> finish casn undetermined `After

let is_after casn =
  match Atomic.get casn with
  | `Undetermined cass as undetermined -> determine casn undetermined cass
  | `After -> true
  | `Before -> false

let cas atom before after =
  let state = { before = after; after; casn = casn_after } in
  let state' = Atomic.get atom.state in
  let value = if is_after state'.casn then state'.after else state'.before in
  value == before && Atomic.compare_and_set atom.state state' state

let commit (T (r, expect, update)) = cas r expect update

let get atom =
  let state = Atomic.get atom.state in
  if is_after state.casn then state.after else state.before

let rec prepare cass casn = function
  | T (atom, before, after) :: cas_list ->
      prepare (CAS (atom.state, { before; after; casn }) :: cass) casn cas_list
  | [] -> cass

let kCAS ?(presort = true) = function
  | [] -> true
  | [ t ] -> commit t
  | cas_list ->
      let cas_list =
        if presort then (
          (* ensure global total order of locations (see section 5 in kCAS paper) *)
          let sorted =
            List.sort
              (fun (T (cas_a, _, _)) (T (cas_b, _, _)) ->
                Int.compare (get_id cas_a) (get_id cas_b))
              cas_list
          in
          (* check for overlapping locations *)
          List.fold_left
            (fun previous_id (T (ref, _, _)) ->
              let current_id = get_id ref in
              if current_id = previous_id then failwith "kcas: location overlap";
              current_id)
            0 sorted
          |> ignore;
          sorted)
        else cas_list
      in

      let casn = Atomic.make `After in
      let cass = prepare [] casn cas_list in
      let undetermined = `Undetermined cass in
      (* The end result is a cyclic data structure, which is why we cannot
         initialize the [casn] atomic directly. *)
      Atomic.set casn undetermined;
      determine casn undetermined cass

let try_map r f =
  let c = get r in
  match f c with
  | None -> Aborted
  | Some v -> if cas r c v then Success c else Failed

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
