(*
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *)

module Backoff = Backoff

module Id = struct
  let id = Atomic.make 1
  let get_unique () = Atomic.fetch_and_add id 1
end

type 'a loc = { state : 'a state Atomic.t; id : int }
and 'a state = { mutable before : 'a; mutable after : 'a; mutable casn : casn }
and cass = CASN : 'a loc * 'a state * cass * cass -> cass | NIL : cass
and casn = status Atomic.t
and status = [ `Undetermined of cass | `After | `Before ]

let casn_after = Atomic.make `After
let casn_before = Atomic.make `Before

let rec release_after = function
  | NIL -> true
  | CASN (_, state, lt, gt) ->
      release_after lt |> ignore;
      state.before <- state.after;
      state.casn <- casn_after;
      release_after gt

let rec release_before = function
  | NIL -> false
  | CASN (_, state, lt, gt) ->
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
  | CASN (loc, desired, lt, gt) as eq ->
      determine casn undetermined true lt
      &&
      let current = Atomic.get loc.state in
      if desired == current then determine casn undetermined skip gt
      else
        let expected = desired.before in
        if
          expected == current.after
          && (current.casn == casn_after || is_after current.casn)
          || expected == current.before
             && (current.casn == casn_before || not (is_after current.casn))
        then
          let status = Atomic.get casn in
          if status != (undetermined :> status) then status == `After
          else if Atomic.compare_and_set loc.state current desired then
            determine casn undetermined skip gt
          else determine casn undetermined skip eq
        else finish casn undetermined `Before

and is_after casn =
  match Atomic.get casn with
  | `Undetermined cass as undetermined -> determine casn undetermined false cass
  | `After -> true
  | `Before -> false

let determine_for_owner casn cass =
  let undetermined = `Undetermined cass in
  (* The end result is a cyclic data structure, which is why we cannot
     initialize the [casn] atomic directly. *)
  Atomic.set casn undetermined;
  determine casn undetermined false cass
  [@@inline]

let overlap () = failwith "kcas: location overlap" [@@inline never]

type splay = Miss : splay | Hit : 'a loc * 'a state -> splay

let rec splay x = function
  | NIL -> (NIL, Miss, NIL)
  | CASN (a, s, l, r) as t ->
      if x < a.id then
        match l with
        | NIL -> (NIL, Miss, t)
        | CASN (pa, ps, ll, lr) ->
            if x < pa.id then
              let lll, n, llr = splay x ll in
              (lll, n, CASN (pa, ps, llr, CASN (a, s, lr, r)))
            else if pa.id < x then
              let lrl, n, lrr = splay x lr in
              (CASN (pa, ps, ll, lrl), n, CASN (a, s, lrr, r))
            else (ll, Hit (pa, ps), CASN (a, s, lr, r))
      else if a.id < x then
        match r with
        | NIL -> (t, Miss, NIL)
        | CASN (pa, ps, rl, rr) ->
            if x < pa.id then
              let rll, n, rlr = splay x rl in
              (CASN (a, s, l, rll), n, CASN (pa, ps, rlr, rr))
            else if pa.id < x then
              let rrl, n, rrr = splay x rr in
              (CASN (pa, ps, CASN (a, s, l, rl), rrl), n, rrr)
            else (CASN (a, s, l, rl), Hit (pa, ps), rr)
      else (l, Hit (a, s), r)

let new_state after = { before = after; after; casn = casn_after } [@@inline]

let eval state' =
  let before' = state'.before and after' = state'.after in
  if before' == after' || is_after state'.casn then after' else before'
  [@@inline]

let rec update_no_alloc backoff loc state set_after =
  let state' = Atomic.get loc.state in
  let before = eval state' in
  set_after state before;
  if Atomic.compare_and_set loc.state state' state then (
    state.before <- state.after;
    before)
  else
    let backoff = Backoff.once backoff in
    update_no_alloc backoff loc state set_after

let cas loc before state =
  let state' = Atomic.get loc.state in
  let before' = state'.before and after' = state'.after in
  ((before == before' && before == after')
  || before == if is_after state'.casn then after' else before')
  && Atomic.compare_and_set loc.state state' state
  [@@inline]

let cmp_state casn before = { before; after = before; casn } [@@inline]

module Loc = struct
  type 'a t = 'a loc

  let make after =
    { state = Atomic.make @@ new_state after; id = Id.get_unique () }

  let get_id loc = loc.id [@@inline]

  let get loc =
    let state = Atomic.get loc.state in
    let before = state.before and after = state.after in
    if before == after || is_after state.casn then after else before

  let compare_and_set loc before after =
    let state = new_state after in
    cas loc before state

  let update ?(backoff = Backoff.default) loc f =
    let state' = Atomic.get loc.state in
    let before = eval state' in
    let state = new_state (f before) in
    if Atomic.compare_and_set loc.state state' state then before
    else
      let backoff = Backoff.once backoff in
      update_no_alloc backoff loc state @@ fun state before ->
      state.after <- f before

  let exchange ?(backoff = Backoff.default) loc value =
    update_no_alloc backoff loc (new_state value) @@ fun _ _ -> ()

  let set ?backoff loc value = exchange ?backoff loc value |> ignore
  let fetch_and_add ?backoff loc n = update ?backoff loc (( + ) n)
  let incr ?backoff loc = fetch_and_add ?backoff loc 1 |> ignore
  let decr ?backoff loc = fetch_and_add ?backoff loc (-1) |> ignore
end

let insert cass loc state =
  let x = loc.id in
  match cass with
  | CASN (a, _, NIL, _) when x < a.id -> CASN (loc, state, NIL, cass)
  | CASN (a, _, _, NIL) when a.id < x -> CASN (loc, state, cass, NIL)
  | _ -> (
      match splay x cass with
      | _, Hit _, _ -> overlap ()
      | l, Miss, r -> CASN (loc, state, l, r))
  [@@inline]

module Op = struct
  type t = CAS : 'a Loc.t * 'a * 'a -> t

  let make_cas loc before after = CAS (loc, before, after) [@@inline]

  let is_on_loc op loc =
    match op with CAS (loc', _, _) -> Obj.magic loc' == loc
    [@@inline]

  let atomic = function
    | CAS (loc, before, after) -> Loc.compare_and_set loc before after

  let atomically = function
    | [] -> true
    | [ op ] -> atomic op
    | first :: rest ->
        let casn = Atomic.make `After in
        let insert cass = function
          | CAS (loc, before, after) -> insert cass loc { before; after; casn }
        in
        let cass =
          match first with
          | CAS (loc, before, after) ->
              CASN (loc, { before; after; casn }, NIL, NIL)
        in
        let cass = List.fold_left insert cass rest in
        determine_for_owner casn cass
end

let get_as0 g loc casn l r =
  let state = Atomic.get loc.state in
  let before = eval state in
  let state = cmp_state casn before in
  ((casn, CASN (loc, state, l, r)), g before)
  [@@inline]

let get_as g loc (casn, cass) =
  match cass with
  | NIL -> get_as0 g loc casn NIL NIL
  | _ -> (
      match splay loc.id cass with
      | l, Miss, r -> get_as0 g loc casn l r
      | l, Hit (_loc', state'), r ->
          let state = Obj.magic state' in
          ((casn, CASN (loc, state, l, r)), g state.after))
  [@@inline]

let update_as0 g loc f casn l r =
  let before = Loc.get loc in
  ((casn, CASN (loc, { before; after = f before; casn }, l, r)), g before)
  [@@inline]

let update_as g loc f casn state' l r =
  let state = Obj.magic state' in
  let current = state.after in
  ((casn, CASN (loc, { state with after = f current }, l, r)), g current)
  [@@inline]

let update_as g loc f (casn, cass) =
  match cass with
  | NIL -> update_as0 g loc f casn NIL NIL
  | CASN (loc', state', l, r) when Obj.magic loc' == loc ->
      update_as g loc f casn state' l r
  | _ -> (
      match splay loc.id cass with
      | l, Miss, r -> update_as0 g loc f casn l r
      | l, Hit (_loc', state'), r -> update_as g loc f casn state' l r)
  [@@inline]

module Tx = struct
  type log = casn * cass
  type 'a t = log -> log * 'a

  let get loc log = get_as Fun.id loc log
  let get_as f loc log = get_as f loc log
  let set loc after log = update_as ignore loc (fun _ -> after) log
  let update loc f log = update_as Fun.id loc f log
  let modify loc f log = update_as ignore loc f log
  let exchange loc after log = update_as Fun.id loc (fun _ -> after) log
  let update_as g loc f log = update_as g loc f log
  let return value log = (log, value)
  let delay uxt log = uxt () log

  let ( let* ) xt xyt log =
    let log, x = xt log in
    xyt x log

  let ( and* ) xt yt log =
    let log, x = xt log in
    let log, y = yt log in
    (log, (x, y))

  let ( let+ ) xt xy log =
    let log, x = xt log in
    (log, xy x)

  let ( and+ ) = ( and* )

  let ( >> ) ut xt log =
    let log, _ = ut log in
    xt log

  let ( >>. ) ut x log =
    let log, _ = ut log in
    (log, x)

  let ( >>= ) = ( let* )
  let map xy xt = ( let+ ) xt xy

  let try_in eyt xyt xt log =
    match xt log with log, x -> xyt x log | exception e -> eyt e log

  let ( <|> ) lhs rhs log = try lhs log with Exit -> rhs log
  let forget _ = raise Exit

  let attempt xt =
    let casn = Atomic.make `After in
    match xt (casn, NIL) with
    | (_, NIL), result -> result
    | (_, CASN (atom, state, NIL, NIL)), result ->
        let before = state.before in
        state.before <- state.after;
        if cas atom before state then result else raise Exit
    | (_, cass), result ->
        if determine_for_owner casn cass then result else raise Exit

  let rec commit backoff xt =
    match attempt xt with
    | result -> result
    | exception Exit ->
        let backoff = Backoff.once backoff in
        commit backoff xt

  let commit ?(backoff = Backoff.default) xt = commit backoff xt
end
