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
and cass = CAS : 'a ref * 'a state * cass * cass -> cass | NIL : cass
and casn = status Atomic.t
and status = [ `Undetermined of cass | `After | `Before ]

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
  | NIL -> true
  | CAS (_, state, lt, gt) ->
      release_after lt |> ignore;
      state.before <- state.after;
      state.casn <- casn_after;
      release_after gt

let rec release_before = function
  | NIL -> false
  | CAS (_, state, lt, gt) ->
      release_before lt |> ignore;
      state.after <- state.before;
      state.casn <- casn_before;
      release_before gt

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

let rec determine casn undetermined skip = function
  | NIL -> skip || finish casn undetermined `After
  | CAS (atom, state, lt, gt) as eq ->
      determine casn undetermined true lt
      &&
      let state' = Atomic.get atom.state in
      if state == state' then determine casn undetermined skip gt
      else
        let before = state.before in
        let before' = state'.before and after' = state'.after in
        if
          (before == before' && before == after')
          || before == if is_after state'.casn then after' else before'
        then
          let status = Atomic.get casn in
          if status != (undetermined :> status) then status == `After
          else if Atomic.compare_and_set atom.state state' state then
            determine casn undetermined skip gt
          else determine casn undetermined skip eq
        else finish casn undetermined `Before

and is_after casn =
  match Atomic.get casn with
  | `Undetermined cass as undetermined -> determine casn undetermined false cass
  | `After -> true
  | `Before -> false

let cas atom before after =
  let state = { before = after; after; casn = casn_after } in
  let state' = Atomic.get atom.state in
  let before' = state'.before and after' = state'.after in
  ((before == before' && before == after')
  || before == if is_after state'.casn then after' else before')
  && Atomic.compare_and_set atom.state state' state

let commit (T (r, expect, update)) = cas r expect update

let get atom =
  let state = Atomic.get atom.state in
  let before = state.before and after = state.after in
  if before == after || is_after state.casn then after else before

let overlap () = failwith "kcas: location overlap" [@@inline never]

let rec splay x = function
  | NIL -> (NIL, NIL)
  | CAS (a, s, l, r) as t ->
      if x < a.id then
        match l with
        | NIL -> (NIL, t)
        | CAS (pa, ps, ll, lr) ->
            if x < pa.id then
              let lll, llr = splay x ll in
              (lll, CAS (pa, ps, llr, CAS (a, s, lr, r)))
            else if pa.id < x then
              let lrl, lrr = splay x lr in
              (CAS (pa, ps, ll, lrl), CAS (a, s, lrr, r))
            else overlap ()
      else if a.id < x then
        match r with
        | NIL -> (t, NIL)
        | CAS (pa, ps, rl, rr) ->
            if x < pa.id then
              let rll, rlr = splay x rl in
              (CAS (a, s, l, rll), CAS (pa, ps, rlr, rr))
            else if pa.id < x then
              let rrl, rrr = splay x rr in
              (CAS (pa, ps, CAS (a, s, l, rl), rrl), rrr)
            else overlap ()
      else overlap ()

let kCAS ?presort:(_ = true) = function
  | [] -> true
  | [ t ] -> commit t
  | T (atom, before, after) :: rest ->
      let casn = Atomic.make `After in
      let insert cass (T (atom, before, after)) =
        let x = atom.id in
        let state = { before; after; casn } in
        match cass with
        | CAS (a, _, NIL, _) when x < a.id -> CAS (atom, state, NIL, cass)
        | CAS (a, _, _, NIL) when a.id < x -> CAS (atom, state, cass, NIL)
        | _ ->
            let l, r = splay x cass in
            CAS (atom, state, l, r)
      in
      let cass =
        List.fold_left insert
          (CAS (atom, { before; after; casn }, NIL, NIL))
          rest
      in
      let undetermined = `Undetermined cass in
      (* The end result is a cyclic data structure, which is why we cannot
         initialize the [casn] atomic directly. *)
      Atomic.set casn undetermined;
      determine casn undetermined false cass

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
