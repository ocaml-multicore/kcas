(*
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *)

(* NOTE: You can adjust comment blocks below to select whether or not to use
   fenceless operations where it is safe to do so.  Fenceless operations have
   been seen to provide significant performance improvements on ARM (Apple
   M1). *)

(**)
external fenceless_get : 'a Atomic.t -> 'a = "%field0"
external fenceless_set : 'a Atomic.t -> 'a -> unit = "%setfield0"
(**)
(*
let fenceless_get = Atomic.get
let fenceless_set = Atomic.set
*)

module Backoff = Backoff

module Id = struct
  let neg_id = Atomic.make (-1)
  let neg_ids n = Atomic.fetch_and_add neg_id (-n) [@@inline]
  let neg_id () = neg_ids 1 [@@inline]
  let nat_id = Atomic.make Int.max_int
  let nat_ids n = Atomic.fetch_and_add nat_id (-n) [@@inline]
  let nat_id () = nat_ids 1 [@@inline]
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

type awaiter = unit -> unit

let resume_awaiter awaiter = awaiter () [@@inline]

let resume_awaiters result = function
  | [] -> result
  | [ awaiter ] ->
      resume_awaiter awaiter;
      result
  | awaiters ->
      List.iter resume_awaiter awaiters;
      result
  [@@inline]

type determined = [ `After | `Before ]

type 'a state = {
  mutable before : 'a;
  mutable after : 'a;
  mutable casn : casn;
  awaiters : awaiter list;
}

and cass =
  | CASN : {
      loc : 'a loc;
      state : 'a state;
      lt : cass;
      gt : cass;
      mutable awaiters : awaiter list;
    }
      -> cass
  | NIL : cass

and casn = status Atomic.t
and status = [ `Undetermined of cass | determined ]

(* NOTE: You can adjust comment blocks below to select whether or not to use an
   unsafe cast to avoid a level of indirection due to [Atomic.t] and reduce the
   size of a location by two words.  This has been seen to provide significant
   performance improvements. *)

(**)
and 'a loc = { mutable _state : 'a state; id : int }

external as_atomic : 'a loc -> 'a state Atomic.t = "%identity"

let make_loc state id = { _state = state; id } [@@inline]
(**)

(*
and 'a loc = { state : 'a state Atomic.t; id : int }

let as_atomic loc = loc.state [@@inline]
let make_loc state id = { state = Atomic.make state; id } [@@inline]
*)

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
  | CASN { state; lt; gt; awaiters; _ } ->
      if lt != NIL then release_after casn lt |> ignore;
      if not (is_cmp casn state) then (
        state.before <- state.after;
        state.casn <- casn_after;
        resume_awaiters () awaiters);
      release_after casn gt

let rec release_before casn = function
  | NIL -> false
  | CASN { state; lt; gt; awaiters; _ } ->
      if lt != NIL then release_before casn lt |> ignore;
      if not (is_cmp casn state) then (
        state.after <- state.before;
        state.casn <- casn_before;
        resume_awaiters () awaiters);
      release_before casn gt

let release casn cass = function
  | `After -> release_after casn cass
  | `Before -> release_before casn cass

let rec verify casn = function
  | NIL -> `After
  | CASN { loc; state; lt; gt; _ } -> (
      if lt == NIL then
        if is_cmp casn state && fenceless_get (as_atomic loc) != state then
          `Before
        else verify casn gt
      else
        match verify casn lt with
        | `After ->
            if is_cmp casn state && fenceless_get (as_atomic loc) != state then
              `Before
            else verify casn gt
        | `Before -> `Before)

let finish casn (`Undetermined cass as undetermined) (status : determined) =
  if Atomic.compare_and_set casn (undetermined :> status) (status :> status)
  then release casn cass status
  else fenceless_get casn == `After

let a_cas = 1
and a_cmp = 2

let a_cas_and_a_cmp = a_cas lor a_cmp

let rec determine casn status = function
  | NIL -> status
  | CASN ({ loc; state; lt; gt; _ } as record) as eq ->
      let status = if lt != NIL then determine casn status lt else status in
      if status < 0 then status
      else
        let current = Atomic.get (as_atomic loc) in
        if state == current then
          determine casn (status lor (1 + Bool.to_int (is_cmp casn state))) gt
        else
          let matches_expected () =
            let expected = state.before in
            expected == current.after
            && (current.casn == casn_after || is_after current.casn)
            || expected == current.before
               && (current.casn == casn_before || not (is_after current.casn))
          in
          if (not (is_cmp casn state)) && matches_expected () then
            match fenceless_get casn with
            | `Undetermined _ ->
                (* We now know that the operation wasn't finished when we read
                   [current], but it is possible that the [loc]ation has been
                   updated since then by some other domain helping us (or even
                   by some later operation). If so, then the [compare_and_set]
                   below fails. Copying the awaiters from [current] is safe in
                   either case, because we know that we have the [current]
                   state that our operation is interested in.  By doing the
                   copying here, we at most duplicate work already done by
                   some other domain. However, it is necessary to do the copy
                   before the [compare_and_set], because afterwards is too
                   late as some other domain might finish the operation after
                   the [compare_and_set] and miss the awaiters. *)
                (match current.awaiters with
                | [] -> ()
                | awaiters -> record.awaiters <- awaiters);
                if Atomic.compare_and_set (as_atomic loc) current state then
                  determine casn (status lor a_cas) gt
                else determine casn status eq
            | #determined -> raise Exit
          else -1

and is_after casn =
  match fenceless_get casn with
  | `Undetermined cass as undetermined -> (
      match determine casn 0 cass with
      | status ->
          finish casn undetermined
            (if a_cas_and_a_cmp = status then verify casn cass
             else if 0 <= status then `After
             else `Before)
      | exception Exit -> fenceless_get casn == `After)
  | `After -> true
  | `Before -> false

let determine_for_owner casn cass =
  let undetermined = `Undetermined cass in
  (* The end result is a cyclic data structure, which is why we cannot
     initialize the [casn] atomic directly. *)
  fenceless_set casn undetermined;
  match determine casn 0 cass with
  | status ->
      if a_cas_and_a_cmp = status then
        (* We only want to [raise Interference] in case it is the verify step
           that fails.  The idea is that in [lock_free] mode the attempt might
           have succeeded as the compared locations would have been set in
           [lock_free] mode preventing interference.  If failure happens before
           the verify step then the [lock_free] mode would have likely also
           failed. *)
        finish casn undetermined (verify casn cass) || raise Mode.Interference
      else
        a_cmp = status
        || finish casn undetermined (if 0 <= status then `After else `Before)
  | exception Exit -> fenceless_get casn == `After
  [@@inline]

let impossible () = failwith "impossible" [@@inline never]
let overlap () = failwith "kcas: location overlap" [@@inline never]
let invalid_retry () = failwith "kcas: invalid use of retry" [@@inline never]

type splay = Miss : splay | Hit : 'a loc * 'a state -> splay

let casn (loc, state, lt, gt) = CASN { loc; state; lt; gt; awaiters = [] }
  [@@inline]

let rec splay ~hit_parent x = function
  | NIL -> (NIL, Miss, NIL)
  | CASN { loc = a; state = s; lt = l; gt = r; _ } as t ->
      if x < a.id && ((not hit_parent) || l != NIL) then
        match l with
        | NIL -> (NIL, Miss, t)
        | CASN { loc = pa; state = ps; lt = ll; gt = lr; _ } ->
            if x < pa.id && ((not hit_parent) || ll != NIL) then
              let lll, n, llr = splay ~hit_parent x ll in
              (lll, n, casn (pa, ps, llr, casn (a, s, lr, r)))
            else if pa.id < x && ((not hit_parent) || lr != NIL) then
              let lrl, n, lrr = splay ~hit_parent x lr in
              (casn (pa, ps, ll, lrl), n, casn (a, s, lrr, r))
            else (ll, Hit (pa, ps), casn (a, s, lr, r))
      else if a.id < x && ((not hit_parent) || r != NIL) then
        match r with
        | NIL -> (t, Miss, NIL)
        | CASN { loc = pa; state = ps; lt = rl; gt = rr; _ } ->
            if x < pa.id && ((not hit_parent) || rl != NIL) then
              let rll, n, rlr = splay ~hit_parent x rl in
              (casn (a, s, l, rll), n, casn (pa, ps, rlr, rr))
            else if pa.id < x && ((not hit_parent) || rr != NIL) then
              let rrl, n, rrr = splay ~hit_parent x rr in
              (casn (pa, ps, casn (a, s, l, rl), rrl), n, rrr)
            else (casn (a, s, l, rl), Hit (pa, ps), rr)
      else (l, Hit (a, s), r)

let new_state after =
  { before = after; after; casn = casn_after; awaiters = [] }
  [@@inline]

let eval state =
  let before = state.before and after = state.after in
  if before == after || is_after state.casn then after else before
  [@@inline]

module Retry = struct
  exception Later

  let later () = raise Later [@@inline never]
  let unless condition = if not condition then later () [@@inline]
end

let add_awaiter loc before awaiter =
  let state_old = fenceless_get (as_atomic loc) in
  let state_new =
    let awaiters = awaiter :: state_old.awaiters in
    { before; after = before; casn = casn_after; awaiters }
  in
  before == eval state_old
  && Atomic.compare_and_set (as_atomic loc) state_old state_new

let[@tail_mod_cons] rec remove_first x' removed = function
  | [] ->
      removed := false;
      []
  | x :: xs -> if x == x' then xs else x :: remove_first x' removed xs

let rec remove_awaiter loc before awaiter =
  let state_old = fenceless_get (as_atomic loc) in
  if before == eval state_old then
    let removed = ref true in
    let awaiters = remove_first awaiter removed state_old.awaiters in
    if !removed then
      let state_new = { before; after = before; casn = casn_after; awaiters } in
      if not (Atomic.compare_and_set (as_atomic loc) state_old state_new) then
        remove_awaiter loc before awaiter

let block loc before =
  let t = Domain_local_await.prepare_for_await () in
  if add_awaiter loc before t.release then (
    try t.await ()
    with cancellation_exn ->
      remove_awaiter loc before t.release;
      raise cancellation_exn)

let rec update_no_alloc backoff loc state f =
  let state' = fenceless_get (as_atomic loc) in
  let before = eval state' in
  match f before with
  | after ->
      state.after <- after;
      if before == after then before
      else if Atomic.compare_and_set (as_atomic loc) state' state then (
        state.before <- after;
        resume_awaiters before state'.awaiters)
      else update_no_alloc (Backoff.once backoff) loc state f
  | exception Retry.Later ->
      block loc before;
      update_no_alloc backoff loc state f

let rec exchange_no_alloc backoff loc state =
  let state' = fenceless_get (as_atomic loc) in
  let before = eval state' in
  if before == state.after then before
  else if Atomic.compare_and_set (as_atomic loc) state' state then
    resume_awaiters before state'.awaiters
  else exchange_no_alloc (Backoff.once backoff) loc state

let is_obstruction_free casn loc =
  fenceless_get casn == (Mode.obstruction_free :> status) && 0 <= loc.id
  [@@inline]

let cas loc before state =
  let state' = fenceless_get (as_atomic loc) in
  let before' = state'.before and after' = state'.after in
  ((before == before' && before == after')
  || before == if is_after state'.casn then after' else before')
  && (before == state.after
     || Atomic.compare_and_set (as_atomic loc) state' state
        && resume_awaiters true state'.awaiters)
  [@@inline]

let inc x = x + 1
let dec x = x - 1

module Loc = struct
  type 'a t = 'a loc

  let make ?(mode = Mode.obstruction_free) after =
    let state = new_state after
    and id =
      if mode == Mode.obstruction_free then Id.nat_id () else Id.neg_id ()
    in
    make_loc state id

  let make_array ?(mode = Mode.obstruction_free) n after =
    assert (0 <= n);
    let state = new_state after
    and id =
      (if mode == Mode.obstruction_free then Id.nat_ids n else Id.neg_ids n)
      - (n - 1)
    in
    Array.init n @@ fun i -> make_loc state (id + i)

  let get_id loc = loc.id [@@inline]
  let get loc = eval (Atomic.get (as_atomic loc))

  let rec get_as f loc =
    let before = get loc in
    match f before with
    | value -> value
    | exception Retry.Later ->
        block loc before;
        get_as f loc

  let get_mode loc =
    if loc.id < 0 then Mode.lock_free else Mode.obstruction_free
    [@@inline]

  let compare_and_set loc before after =
    let state = new_state after in
    cas loc before state

  let update ?(backoff = Backoff.default) loc f =
    let state' = fenceless_get (as_atomic loc) in
    let before = eval state' in
    match f before with
    | after ->
        if before == after then before
        else
          let state = new_state after in
          if Atomic.compare_and_set (as_atomic loc) state' state then
            resume_awaiters before state'.awaiters
          else update_no_alloc (Backoff.once backoff) loc state f
    | exception Retry.Later ->
        let state = new_state before in
        block loc before;
        update_no_alloc backoff loc state f

  let modify ?backoff loc f = update ?backoff loc f |> ignore [@@inline]

  let exchange ?(backoff = Backoff.default) loc value =
    exchange_no_alloc backoff loc (new_state value)

  let set ?backoff loc value = exchange ?backoff loc value |> ignore
  let fetch_and_add ?backoff loc n = update ?backoff loc (( + ) n)
  let incr ?backoff loc = update ?backoff loc inc |> ignore
  let decr ?backoff loc = update ?backoff loc dec |> ignore

  let has_awaiters loc =
    let state = Atomic.get (as_atomic loc) in
    state.awaiters != []

  let fenceless_get loc = eval (fenceless_get (as_atomic loc))
end

let insert cass loc state =
  let x = loc.id in
  match cass with
  | CASN { loc = a; lt = NIL; _ } when x < a.id ->
      CASN { loc; state; lt = NIL; gt = cass; awaiters = [] }
  | CASN { loc = a; gt = NIL; _ } when a.id < x ->
      CASN { loc; state; lt = cass; gt = NIL; awaiters = [] }
  | _ -> (
      match splay ~hit_parent:false x cass with
      | _, Hit _, _ -> overlap ()
      | lt, Miss, gt -> CASN { loc; state; lt; gt; awaiters = [] })
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
              if before == after && is_obstruction_free casn loc then
                let state = fenceless_get (as_atomic loc) in
                before == eval state && run (insert cass loc state) rest
              else
                run
                  (insert cass loc { before; after; casn; awaiters = [] })
                  rest
        in
        let (CAS (loc, before, after)) = first in
        if before == after && is_obstruction_free casn loc then
          let state = fenceless_get (as_atomic loc) in
          before == eval state
          && run (CASN { loc; state; lt = NIL; gt = NIL; awaiters = [] }) rest
        else
          let state = { before; after; casn; awaiters = [] } in
          run (CASN { loc; state; lt = NIL; gt = NIL; awaiters = [] }) rest
end

module Xt = struct
  type 'x t = {
    mutable casn : casn;
    mutable cass : cass;
    mutable validate_countdown : int;
    mutable validate_period : int;
    mutable post_commit : Action.t;
  }

  let rec validate casn = function
    | NIL -> ()
    | CASN { loc; state; lt; gt; _ } ->
        if lt != NIL then validate casn lt;
        let before = if is_cmp casn state then eval state else state.before in
        if before != eval (fenceless_get (as_atomic loc)) then Retry.later ();
        validate casn gt

  let validate xt =
    let p = xt.validate_period * 2 in
    xt.validate_countdown <- p;
    xt.validate_period <- p;
    validate xt.casn xt.cass
    [@@inline never]

  let maybe_validate xt =
    let c = xt.validate_countdown - 1 in
    if 0 < c then xt.validate_countdown <- c else validate xt
    [@@inline]

  let update0 loc f xt lt gt =
    let state = fenceless_get (as_atomic loc) in
    let before = eval state in
    let after = f before in
    let state =
      if before == after && is_obstruction_free xt.casn loc then state
      else { before; after; casn = xt.casn; awaiters = [] }
    in
    xt.cass <- CASN { loc; state; lt; gt; awaiters = [] };
    before
    [@@inline]

  let update loc f xt state' lt gt =
    let state = Obj.magic state' in
    if is_cmp xt.casn state then (
      let before = eval state in
      let after = f before in
      let state =
        if before == after then state
        else { before; after; casn = xt.casn; awaiters = [] }
      in
      xt.cass <- CASN { loc; state; lt; gt; awaiters = [] };
      before)
    else
      let current = state.after in
      let state = { state with after = f current } in
      xt.cass <- CASN { loc; state; lt; gt; awaiters = [] };
      current
    [@@inline]

  let update loc f xt =
    maybe_validate xt;
    let x = loc.id in
    match xt.cass with
    | NIL -> update0 loc f xt NIL NIL
    | CASN { loc = a; lt = NIL; _ } as cass when x < a.id ->
        update0 loc f xt NIL cass
    | CASN { loc = a; gt = NIL; _ } as cass when a.id < x ->
        update0 loc f xt cass NIL
    | CASN { loc = a; state; lt; gt; _ } when Obj.magic a == loc ->
        update loc f xt state lt gt
    | cass -> (
        match splay ~hit_parent:false x cass with
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
  let incr ~xt loc = update loc inc xt |> ignore
  let decr ~xt loc = update loc dec xt |> ignore
  let update ~xt loc f = update loc (protect xt f) xt
  let swap ~xt l1 l2 = set ~xt l1 @@ exchange ~xt l2 @@ get ~xt l1

  let to_blocking ~xt tx =
    match tx ~xt with None -> Retry.later () | Some value -> value
    [@@inline]

  let to_nonblocking ~xt tx =
    match tx ~xt with value -> Some value | exception Retry.Later -> None
    [@@inline]

  let post_commit ~xt action =
    xt.post_commit <- Action.append action xt.post_commit

  let is_in_log ~xt loc =
    let x = loc.id in
    match xt.cass with
    | NIL -> false
    | CASN { loc = a; lt = NIL; _ } when x < a.id -> false
    | CASN { loc = a; gt = NIL; _ } when a.id < x -> false
    | CASN { loc = a; _ } when Obj.magic a == loc -> true
    | cass -> (
        match splay ~hit_parent:true x cass with
        | lt, Hit (a, state), gt ->
            xt.cass <- CASN { loc = a; state; lt; gt; awaiters = [] };
            Obj.magic a == loc
        | _, Miss, _ -> impossible ())

  type 'a tx = { tx : 'x. xt:'x t -> 'a } [@@unboxed]

  let call { tx } = tx [@@inline]

  let rec add_awaiters awaiter casn = function
    | NIL as cont -> cont
    | CASN { loc; state; lt; gt; _ } as stop -> (
        match if lt == NIL then lt else add_awaiters awaiter casn lt with
        | NIL ->
            if
              add_awaiter loc
                (if is_cmp casn state then eval state else state.before)
                awaiter
            then add_awaiters awaiter casn gt
            else stop
        | CASN _ as stop -> stop)

  let rec remove_awaiters awaiter casn stop = function
    | NIL -> ()
    | CASN { loc; state; lt; gt; _ } as current ->
        if lt != NIL then remove_awaiters awaiter casn stop lt;
        if current != stop then (
          remove_awaiter loc
            (if is_cmp casn state then eval state else state.before)
            awaiter;
          remove_awaiters awaiter casn stop gt)

  let initial_validate_period = 16

  let reset_quick xt =
    xt.cass <- NIL;
    xt.validate_countdown <- initial_validate_period;
    xt.validate_period <- initial_validate_period;
    xt.post_commit <- Action.noop;
    xt
    [@@inline]

  let reset mode xt =
    xt.casn <- Atomic.make (mode :> status);
    reset_quick xt

  let rec commit backoff mode xt tx =
    match tx ~xt with
    | result -> (
        match xt.cass with
        | NIL -> Action.run xt.post_commit result
        | CASN { loc; state; lt = NIL; gt = NIL; awaiters = _ } ->
            if is_cmp xt.casn state then Action.run xt.post_commit result
            else
              let before = state.before in
              state.before <- state.after;
              state.casn <- casn_after;
              if cas loc before state then Action.run xt.post_commit result
              else commit (Backoff.once backoff) mode (reset_quick xt) tx
        | cass -> (
            match determine_for_owner xt.casn cass with
            | true -> Action.run xt.post_commit result
            | false -> commit (Backoff.once backoff) mode (reset mode xt) tx
            | exception Mode.Interference ->
                commit (Backoff.once backoff) Mode.lock_free
                  (reset Mode.lock_free xt) tx))
    | exception Retry.Later -> (
        if xt.cass == NIL then invalid_retry ();
        let t = Domain_local_await.prepare_for_await () in
        match add_awaiters t.release xt.casn xt.cass with
        | NIL -> (
            match t.await () with
            | () ->
                remove_awaiters t.release xt.casn NIL xt.cass;
                commit (Backoff.reset backoff) mode (reset_quick xt) tx
            | exception cancellation_exn ->
                remove_awaiters t.release xt.casn NIL xt.cass;
                raise cancellation_exn)
        | CASN _ as stop ->
            remove_awaiters t.release xt.casn stop xt.cass;
            commit (Backoff.once backoff) mode (reset_quick xt) tx)

  let commit ?(backoff = Backoff.default) ?(mode = Mode.obstruction_free) tx =
    let casn = Atomic.make (mode :> status)
    and cass = NIL
    and validate_countdown = initial_validate_period
    and validate_period = initial_validate_period
    and post_commit = Action.noop in
    let xt = { casn; cass; validate_countdown; post_commit; validate_period } in
    commit backoff mode xt tx.tx
    [@@inline]
end
