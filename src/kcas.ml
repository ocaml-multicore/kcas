(*
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *)

module Backoff = Backoff

module Id = struct
  let id = Atomic.make 1
  let get_unique () = Atomic.fetch_and_add id 1
end

module Action : sig
  type t

  val noop : t
  val append : (unit -> unit) -> t -> t
  val run : t -> 'a -> 'a
end = struct
  type t = unit -> unit

  let noop = Fun.id

  let append action t = if t == noop then action else fun x -> action (t x)
    [@@inline]

  let run t x =
    t ();
    x
    [@@inline]
end

type determined = [ `After | `Before ]

type 'a loc = { state : 'a state Atomic.t; id : int }
and 'a state = { mutable before : 'a; mutable after : 'a; mutable casn : casn }
and cass = CASN : 'a loc * 'a state * cass * cass -> cass | NIL : cass
and casn = status Atomic.t
and status = [ `Undetermined of cass | determined ]

let is_cmp casn state = state.casn != casn [@@inline]

module Mode = struct
  type t = determined

  let lock_free = (`After :> t)
  let obstruction_free = (`Before :> t)

  exception Interference
end

let casn_after = Atomic.make `After
let casn_before = Atomic.make `Before

let rec release_after casn = function
  | NIL -> true
  | CASN (_, state, lt, gt) ->
      if lt != NIL then release_after casn lt |> ignore;
      if not (is_cmp casn state) then (
        state.before <- state.after;
        state.casn <- casn_after);
      release_after casn gt

let rec release_before casn = function
  | NIL -> false
  | CASN (_, state, lt, gt) ->
      if lt != NIL then release_before casn lt |> ignore;
      if not (is_cmp casn state) then (
        state.after <- state.before;
        state.casn <- casn_before);
      release_before casn gt

let release casn cass = function
  | `After -> release_after casn cass
  | `Before -> release_before casn cass

let rec verify casn = function
  | NIL -> `After
  | CASN (atom, desired, lt, gt) -> (
      if lt == NIL then
        if is_cmp casn desired && Atomic.get atom.state != desired then `Before
        else verify casn gt
      else
        match verify casn lt with
        | `After ->
            if is_cmp casn desired && Atomic.get atom.state != desired then
              `Before
            else verify casn gt
        | `Before -> `Before)

let finish casn (`Undetermined cass as undetermined) (status : determined) =
  if Atomic.compare_and_set casn (undetermined :> status) (status :> status)
  then release casn cass status
  else Atomic.get casn == `After

let exit _ = raise Exit [@@inline never]

let rec determine casn action = function
  | NIL -> action
  | CASN (loc, desired, lt, gt) as eq -> (
      match if lt != NIL then determine casn action lt else action with
      | `Before -> `Before
      | (`After | `Verify) as action ->
          let current = Atomic.get loc.state in
          if desired == current then
            let action = if is_cmp casn desired then `Verify else action in
            determine casn action gt
          else
            let matches_expected () =
              let expected = desired.before in
              expected == current.after
              && (current.casn == casn_after || is_after current.casn)
              || expected == current.before
                 && (current.casn == casn_before || not (is_after current.casn))
            in
            if (not (is_cmp casn desired)) && matches_expected () then
              match Atomic.get casn with
              | `Undetermined _ ->
                  if Atomic.compare_and_set loc.state current desired then
                    determine casn action gt
                  else determine casn action eq
              | #determined -> exit ()
            else `Before)

and is_after casn =
  match Atomic.get casn with
  | `Undetermined cass as undetermined -> (
      match determine casn `After cass with
      | `Verify -> finish casn undetermined (verify casn cass)
      | #determined as status -> finish casn undetermined status
      | exception Exit -> Atomic.get casn == `After)
  | `After -> true
  | `Before -> false

let determine_for_owner casn cass =
  let undetermined = `Undetermined cass in
  (* The end result is a cyclic data structure, which is why we cannot
     initialize the [casn] atomic directly. *)
  Atomic.set casn undetermined;
  match determine casn `After cass with
  | `Verify ->
      (* We only want to [raise Interference] in case it is the verify step that
         fails.  The idea is that in [lock_free] mode the attempt might have
         succeeded as the compared locations would have been set in [lock_free]
         mode preventing interference.  If failure happens before the verify
         step then the [lock_free] mode would have likely also failed. *)
      finish casn undetermined (verify casn cass) || raise Mode.Interference
  | #determined as status -> finish casn undetermined status
  | exception Exit -> Atomic.get casn == `After
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

let is_obstruction_free casn =
  Atomic.get casn == (Mode.obstruction_free :> status)
  [@@inline]

let cas loc before state =
  let state' = Atomic.get loc.state in
  let before' = state'.before and after' = state'.after in
  ((before == before' && before == after')
  || before == if is_after state'.casn then after' else before')
  && Atomic.compare_and_set loc.state state' state
  [@@inline]

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
  let make_cmp loc expected = CAS (loc, expected, expected) [@@inline]

  let is_on_loc op loc =
    match op with CAS (loc', _, _) -> Obj.magic loc' == loc
    [@@inline]

  let get_id = function CAS (loc, _, _) -> loc.id [@@inline]

  let atomic = function
    | CAS (loc, before, after) ->
        if before == after then Loc.get loc == before
        else Loc.compare_and_set loc before after

  let atomically ?(mode = Mode.lock_free) = function
    | [] -> true
    | [ op ] -> atomic op
    | first :: rest ->
        let casn = Atomic.make (mode :> status) in
        let rec run cass = function
          | [] -> determine_for_owner casn cass
          | CAS (loc, before, after) :: rest ->
              if before == after && is_obstruction_free casn then
                let state = Atomic.get loc.state in
                before == eval state && run (insert cass loc state) rest
              else run (insert cass loc { before; after; casn }) rest
        in
        let (CAS (loc, before, after)) = first in
        if before == after && is_obstruction_free casn then
          let state = Atomic.get loc.state in
          before == eval state && run (CASN (loc, state, NIL, NIL)) rest
        else run (CASN (loc, { before; after; casn }, NIL, NIL)) rest
end

let update_as0 g loc f casn post_commit l r =
  let state = Atomic.get loc.state in
  let before = eval state in
  let after = f before in
  let state =
    if before == after && is_obstruction_free casn then state
    else { before; after; casn }
  in
  ((casn, CASN (loc, state, l, r), post_commit), g before)
  [@@inline]

let update_as g loc f casn post_commit state' l r =
  let state = Obj.magic state' in
  if is_cmp casn state then
    let before = eval state in
    let after = f before in
    let state = if before == after then state else { before; after; casn } in
    ((casn, CASN (loc, state, l, r), post_commit), g before)
  else
    let current = state.after in
    ( (casn, CASN (loc, { state with after = f current }, l, r), post_commit),
      g current )
  [@@inline]

let update_as g loc f (casn, cass, post_commit) =
  let x = loc.id in
  match cass with
  | NIL -> update_as0 g loc f casn post_commit NIL NIL
  | CASN (a, _, NIL, _) as cass when x < a.id ->
      update_as0 g loc f casn post_commit NIL cass
  | CASN (a, _, _, NIL) as cass when a.id < x ->
      update_as0 g loc f casn post_commit cass NIL
  | CASN (loc', state', l, r) when Obj.magic loc' == loc ->
      update_as g loc f casn post_commit state' l r
  | _ -> (
      match splay x cass with
      | l, Miss, r -> update_as0 g loc f casn post_commit l r
      | l, Hit (_loc', state'), r ->
          update_as g loc f casn post_commit state' l r)
  [@@inline]

let attempt (mode : Mode.t) tx =
  let casn = Atomic.make (mode :> status) in
  match tx (casn, NIL, Action.noop) with
  | (_, NIL, post_commit), result -> Action.run post_commit result
  | (_, CASN (loc, state, NIL, NIL), post_commit), result ->
      if is_cmp casn state then Action.run post_commit result
      else
        let before = state.before in
        state.before <- state.after;
        if cas loc before state then Action.run post_commit result else exit ()
  | (_, cass, post_commit), result ->
      if determine_for_owner casn cass then Action.run post_commit result
      else exit ()

let rec commit backoff mode tx =
  match attempt mode tx with
  | result -> result
  | exception Mode.Interference ->
      let backoff = Backoff.once backoff in
      commit backoff Mode.lock_free tx
  | exception Exit ->
      let backoff = Backoff.once backoff in
      commit backoff mode tx

module Tx = struct
  type log = casn * cass * Action.t
  type 'a t = log -> log * 'a

  let get loc log = update_as Fun.id loc Fun.id log
  let get_as f loc log = update_as f loc Fun.id log
  let set loc after log = update_as ignore loc (fun _ -> after) log
  let update loc f log = update_as Fun.id loc f log
  let modify loc f log = update_as ignore loc f log

  let compare_and_swap loc before after log =
    update_as Fun.id loc
      (fun actual -> if actual == before then after else actual)
      log

  let exchange_as g loc after log = update_as g loc (fun _ -> after) log
  let exchange loc after log = update_as Fun.id loc (fun _ -> after) log
  let fetch_and_add loc n log = update_as Fun.id loc (( + ) n) log
  let incr loc log = update_as ignore loc (( + ) 1) log
  let decr loc log = update_as ignore loc (( + ) (-1)) log
  let update_as g loc f log = update_as g loc f log

  let swap l1 l2 log =
    let log, x1 = get l1 log in
    let log, x2 = exchange l2 x1 log in
    set l1 x2 log

  let return value log = (log, value)
  let delay uxt log = uxt () log

  let post_commit action (casn, cass, post_commit) =
    ((casn, cass, Action.append action post_commit), ())

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
  let forget = exit
  let attempt ?(mode = Mode.lock_free) tx = attempt mode tx

  let commit ?(backoff = Backoff.default) ?(mode = Mode.obstruction_free) tx =
    commit backoff mode tx
end

module Xt = struct
  type 'x t = {
    casn : casn;
    mutable cass : cass;
    mutable post_commit : Action.t;
  }

  let update0 loc f xt l r =
    let state = Atomic.get loc.state in
    let before = eval state in
    let after = f before in
    let state =
      if before == after && is_obstruction_free xt.casn then state
      else { before; after; casn = xt.casn }
    in
    xt.cass <- CASN (loc, state, l, r);
    before
    [@@inline]

  let update loc f xt state' l r =
    let state = Obj.magic state' in
    if is_cmp xt.casn state then (
      let before = eval state in
      let after = f before in
      let state =
        if before == after then state else { before; after; casn = xt.casn }
      in
      xt.cass <- CASN (loc, state, l, r);
      before)
    else
      let current = state.after in
      xt.cass <- CASN (loc, { state with after = f current }, l, r);
      current
    [@@inline]

  let update loc f xt =
    let x = loc.id in
    match xt.cass with
    | NIL -> update0 loc f xt NIL NIL
    | CASN (a, _, NIL, _) as cass when x < a.id -> update0 loc f xt NIL cass
    | CASN (a, _, _, NIL) as cass when a.id < x -> update0 loc f xt cass NIL
    | CASN (loc', state', l, r) when Obj.magic loc' == loc ->
        update loc f xt state' l r
    | cass -> (
        match splay x cass with
        | l, Miss, r -> update0 loc f xt l r
        | l, Hit (_loc', state'), r -> update loc f xt state' l r)
    [@@inline]

  let protect xt f x =
    let cass = xt.cass in
    let y = f x in
    assert (xt.cass == cass);
    y
    [@@inline]

  let get ~xt loc = update loc Fun.id xt
  let set ~xt loc after = update loc (fun _ -> after) xt |> ignore
  let modify ~xt loc f = update loc (protect xt f) xt |> ignore

  let compare_and_swap ~xt loc before after =
    update loc (fun actual -> if actual == before then after else actual) xt

  let exchange ~xt loc after = update loc (fun _ -> after) xt
  let fetch_and_add ~xt loc n = update loc (( + ) n) xt
  let incr ~xt loc = fetch_and_add ~xt loc 1 |> ignore
  let decr ~xt loc = fetch_and_add ~xt loc (-1) |> ignore
  let update ~xt loc f = update loc (protect xt f) xt
  let swap ~xt l1 l2 = set ~xt l1 @@ exchange ~xt l2 @@ get ~xt l1

  let post_commit ~xt action =
    xt.post_commit <- Action.append action xt.post_commit

  type 'a tx = { tx : 'x. xt:'x t -> 'a } [@@unboxed]

  let call { tx } = tx [@@inline]

  let attempt (mode : Mode.t) tx =
    let xt =
      let casn = Atomic.make (mode :> status)
      and cass = NIL
      and post_commit = Action.noop in
      { casn; cass; post_commit }
    in
    let result = tx ~xt in
    match xt.cass with
    | NIL -> Action.run xt.post_commit result
    | CASN (loc, state, NIL, NIL) ->
        if is_cmp xt.casn state then Action.run xt.post_commit result
        else
          let before = state.before in
          state.before <- state.after;
          if cas loc before state then Action.run xt.post_commit result
          else exit ()
    | cass ->
        if determine_for_owner xt.casn cass then
          Action.run xt.post_commit result
        else exit ()

  let rec commit backoff mode tx =
    match attempt mode tx with
    | result -> result
    | exception Mode.Interference ->
        commit (Backoff.once backoff) Mode.lock_free tx
    | exception Exit -> commit (Backoff.once backoff) mode tx

  let commit ?(backoff = Backoff.default) ?(mode = Mode.obstruction_free) tx =
    commit backoff mode tx.tx
    [@@inline]

  let attempt ?(mode = Mode.lock_free) tx = attempt mode tx.tx [@@inline]

  let of_tx tx ~xt =
    let (_, cass, post_commit), x = tx (xt.casn, xt.cass, xt.post_commit) in
    xt.cass <- cass;
    xt.post_commit <- post_commit;
    x

  let to_tx tx (casn, cass, post_commit) =
    let xt = { casn; cass; post_commit } in
    let x = tx.tx ~xt in
    ((xt.casn, xt.cass, xt.post_commit), x)
end
