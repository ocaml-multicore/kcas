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
let is_cas casn state = state.casn == casn [@@inline]

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
        (* Fenceless is safe as [finish] has a fence after. *)
        if is_cmp casn state && fenceless_get (as_atomic loc) != state then
          `Before
        else verify casn gt
      else
        match verify casn lt with
        | `After ->
            (* Fenceless is safe as [finish] has a fence after. *)
            if is_cmp casn state && fenceless_get (as_atomic loc) != state then
              `Before
            else verify casn gt
        | `Before -> `Before)

let finish casn (`Undetermined cass as undetermined) (status : determined) =
  if Atomic.compare_and_set casn (undetermined :> status) (status :> status)
  then release casn cass status
  else
    (* Fenceless is safe as we have a fence above. *)
    fenceless_get casn == `After

let a_cmp = 1
let a_cas = 2
let a_cmp_followed_by_a_cas = 4

let rec determine casn status = function
  | NIL -> status
  | CASN ({ loc; state; lt; gt; _ } as record) as eq ->
      let status = if lt != NIL then determine casn status lt else status in
      if status < 0 then status
      else
        let current = Atomic.get (as_atomic loc) in
        if state == current then
          let a_cas_or_a_cmp = 1 + Bool.to_int (is_cas casn state) in
          let a_cmp_followed_by_a_cas = a_cas_or_a_cmp * 2 land (status * 4) in
          determine casn
            (status lor a_cas_or_a_cmp lor a_cmp_followed_by_a_cas)
            gt
        else
          let matches_expected () =
            let expected = state.before in
            expected == current.after
            && (current.casn == casn_after || is_after current.casn)
            || expected == current.before
               && (current.casn == casn_before || not (is_after current.casn))
          in
          if (not (is_cmp casn state)) && matches_expected () then
            (* Fenceless is safe as there are fences before and after. *)
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
                  let a_cmp_followed_by_a_cas = a_cas * 2 land (status * 4) in
                  determine casn
                    (status lor a_cas lor a_cmp_followed_by_a_cas)
                    gt
                else determine casn status eq
            | #determined -> raise Exit
          else -1

and is_after casn =
  (* Fenceless at most gives old [Undetermined] and causes extra work. *)
  match fenceless_get casn with
  | `Undetermined cass as undetermined -> (
      match determine casn 0 cass with
      | status ->
          finish casn undetermined
            (if a_cmp_followed_by_a_cas < status then verify casn cass
             else if 0 <= status then `After
             else `Before)
      | exception Exit ->
          (* Fenceless is safe as there was a fence before. *)
          fenceless_get casn == `After)
  | `After -> true
  | `Before -> false

let determine_for_owner casn cass =
  (* The end result is a cyclic data structure, which is why we cannot
     initialize the [casn] atomic directly. *)
  let undetermined = `Undetermined cass in
  (* Fenceless is safe as [casn] is private at this point. *)
  fenceless_set casn undetermined;
  match determine casn 0 cass with
  | status ->
      if a_cmp_followed_by_a_cas < status then
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
  | exception Exit ->
      (* Fenceless is safe as there was a fence before. *)
      fenceless_get casn == `After
  [@@inline]

let impossible () = failwith "impossible" [@@inline never]
let overlap () = failwith "kcas: location overlap" [@@inline never]
let invalid_retry () = failwith "kcas: invalid use of retry" [@@inline never]

type splay = Miss : splay | Hit : 'a loc * 'a state -> splay

let make_casn loc state lt gt = CASN { loc; state; lt; gt; awaiters = [] }
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
              (lll, n, make_casn pa ps llr (make_casn a s lr r))
            else if pa.id < x && ((not hit_parent) || lr != NIL) then
              let lrl, n, lrr = splay ~hit_parent x lr in
              (make_casn pa ps ll lrl, n, make_casn a s lrr r)
            else (ll, Hit (pa, ps), make_casn a s lr r)
      else if a.id < x && ((not hit_parent) || r != NIL) then
        match r with
        | NIL -> (t, Miss, NIL)
        | CASN { loc = pa; state = ps; lt = rl; gt = rr; _ } ->
            if x < pa.id && ((not hit_parent) || rl != NIL) then
              let rll, n, rlr = splay ~hit_parent x rl in
              (make_casn a s l rll, n, make_casn pa ps rlr rr)
            else if pa.id < x && ((not hit_parent) || rr != NIL) then
              let rrl, n, rrr = splay ~hit_parent x rr in
              (make_casn pa ps (make_casn a s l rl) rrl, n, rrr)
            else (make_casn a s l rl, Hit (pa, ps), rr)
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

  exception Invalid

  let invalid () = raise Invalid [@@inline never]
end

let add_awaiter loc before awaiter =
  (* Fenceless is safe as we have fence after. *)
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
  (* Fenceless is safe as we have fence after. *)
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
  (* Fenceless is safe as we have had a fence before if needed and there is a fence after. *)
  let state_old = fenceless_get (as_atomic loc) in
  let before = eval state_old in
  match f before with
  | after ->
      state.after <- after;
      if before == after then before
      else if Atomic.compare_and_set (as_atomic loc) state_old state then (
        state.before <- after;
        resume_awaiters before state_old.awaiters)
      else update_no_alloc (Backoff.once backoff) loc state f
  | exception Retry.Later ->
      block loc before;
      update_no_alloc backoff loc state f

let update_with_state backoff loc f state_old =
  let before = eval state_old in
  match f before with
  | after ->
      if before == after then before
      else
        let state = new_state after in
        if Atomic.compare_and_set (as_atomic loc) state_old state then
          resume_awaiters before state_old.awaiters
        else update_no_alloc (Backoff.once backoff) loc state f
  | exception Retry.Later ->
      let state = new_state before in
      block loc before;
      update_no_alloc backoff loc state f

let rec exchange_no_alloc backoff loc state =
  let state_old = Atomic.get (as_atomic loc) in
  let before = eval state_old in
  if before == state.after then before
  else if Atomic.compare_and_set (as_atomic loc) state_old state then
    resume_awaiters before state_old.awaiters
  else exchange_no_alloc (Backoff.once backoff) loc state

let is_obstruction_free casn loc =
  (* Fenceless is safe as we are accessing a private location. *)
  fenceless_get casn == (Mode.obstruction_free :> status) && 0 <= loc.id
  [@@inline]

let cas_with_state loc before state state_old =
  let before' = state_old.before and after' = state_old.after in
  ((before == before' && before == after')
  || before == if is_after state_old.casn then after' else before')
  && (before == state.after
     || Atomic.compare_and_set (as_atomic loc) state_old state
        && resume_awaiters true state_old.awaiters)
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
    let state_old = Atomic.get (as_atomic loc) in
    cas_with_state loc before state state_old

  let fenceless_update ?(backoff = Backoff.default) loc f =
    update_with_state backoff loc f (fenceless_get (as_atomic loc))

  let fenceless_modify ?backoff loc f =
    fenceless_update ?backoff loc f |> ignore
    [@@inline]

  let update ?(backoff = Backoff.default) loc f =
    update_with_state backoff loc f (Atomic.get (as_atomic loc))

  let modify ?backoff loc f = update ?backoff loc f |> ignore [@@inline]

  let exchange ?(backoff = Backoff.default) loc value =
    exchange_no_alloc backoff loc (new_state value)

  let set ?backoff loc value = exchange ?backoff loc value |> ignore

  let fetch_and_add ?backoff loc n =
    if n = 0 then get loc
    else
      (* Fenceless is safe as we always update. *)
      fenceless_update ?backoff loc (( + ) n)

  let incr ?backoff loc =
    (* Fenceless is safe as we always update. *)
    fenceless_update ?backoff loc inc |> ignore

  let decr ?backoff loc =
    (* Fenceless is safe as we always update. *)
    fenceless_update ?backoff loc dec |> ignore

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
                (* Fenceless is safe as there are fences in [determine]. *)
                let state = fenceless_get (as_atomic loc) in
                before == eval state && run (insert cass loc state) rest
              else
                run
                  (insert cass loc { before; after; casn; awaiters = [] })
                  rest
        in
        let (CAS (loc, before, after)) = first in
        if before == after && is_obstruction_free casn loc then
          (* Fenceless is safe as there are fences in [determine]. *)
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

  let validate_one casn loc state =
    let before = if is_cmp casn state then eval state else state.before in
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    if before != eval (fenceless_get (as_atomic loc)) then Retry.invalid ()
    [@@inline]

  let rec validate_all casn = function
    | NIL -> ()
    | CASN { loc; state; lt; gt; _ } ->
        if lt != NIL then validate_all casn lt;
        validate_one casn loc state;
        validate_all casn gt

  let validate_log xt =
    let p = xt.validate_period * 2 in
    xt.validate_countdown <- p;
    xt.validate_period <- p;
    validate_all xt.casn xt.cass
    [@@inline never]

  let maybe_validate_log xt =
    let c = xt.validate_countdown - 1 in
    if 0 < c then xt.validate_countdown <- c else validate_log xt
    [@@inline]

  let update0 loc f xt lt gt =
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    let state = fenceless_get (as_atomic loc) in
    let before = eval state in
    match f before with
    | after ->
        let state =
          if before == after && is_obstruction_free xt.casn loc then state
          else { before; after; casn = xt.casn; awaiters = [] }
        in
        xt.cass <- CASN { loc; state; lt; gt; awaiters = [] };
        before
    | exception exn ->
        xt.cass <- CASN { loc; state; lt; gt; awaiters = [] };
        raise exn
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

  let unsafe_update ~xt loc f =
    maybe_validate_log xt;
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

  let get ~xt loc = unsafe_update ~xt loc Fun.id
  let set ~xt loc after = unsafe_update ~xt loc (fun _ -> after) |> ignore
  let modify ~xt loc f = unsafe_update ~xt loc (protect xt f) |> ignore

  let compare_and_swap ~xt loc before after =
    unsafe_update ~xt loc (fun actual ->
        if actual == before then after else actual)

  let exchange ~xt loc after = unsafe_update ~xt loc (fun _ -> after)
  let fetch_and_add ~xt loc n = unsafe_update ~xt loc (( + ) n)
  let incr ~xt loc = unsafe_update ~xt loc inc |> ignore
  let decr ~xt loc = unsafe_update ~xt loc dec |> ignore
  let update ~xt loc f = unsafe_update ~xt loc (protect xt f)
  let swap ~xt l1 l2 = set ~xt l1 @@ exchange ~xt l2 @@ get ~xt l1
  let unsafe_modify ~xt loc f = unsafe_update ~xt loc f |> ignore
  let unsafe_update ~xt loc f = unsafe_update ~xt loc f

  let to_blocking ~xt tx =
    match tx ~xt with None -> Retry.later () | Some value -> value
    [@@inline]

  let to_nonblocking ~xt tx =
    match tx ~xt with value -> Some value | exception Retry.Later -> None
    [@@inline]

  let post_commit ~xt action =
    xt.post_commit <- Action.append action xt.post_commit

  let validate ~xt loc =
    let x = loc.id in
    match xt.cass with
    | NIL -> ()
    | CASN { loc = a; lt = NIL; _ } when x < a.id -> ()
    | CASN { loc = a; gt = NIL; _ } when a.id < x -> ()
    | CASN { loc = a; state; _ } when Obj.magic a == loc ->
        validate_one xt.casn a state
    | cass -> (
        match splay ~hit_parent:true x cass with
        | lt, Hit (a, state), gt ->
            xt.cass <- CASN { loc = a; state; lt; gt; awaiters = [] };
            if Obj.magic a == loc then validate_one xt.casn a state
        | _, Miss, _ -> impossible ())

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

  let rec rollback casn cass_snap cass =
    if cass_snap == cass then cass
    else
      match cass with
      | NIL -> NIL
      | CASN { loc; state; lt; gt; _ } -> (
          match splay ~hit_parent:false loc.id cass_snap with
          | lt_mark, Miss, gt_mark ->
              let lt = rollback casn lt_mark lt
              and gt = rollback casn gt_mark gt in
              let state =
                if is_cmp casn state then state
                else
                  (* Fenceless is safe inside transactions as each log update has a fence. *)
                  let current = fenceless_get (as_atomic loc) in
                  if state.before != eval current then Retry.invalid ()
                  else current
              in
              CASN { loc; state; lt; gt; awaiters = [] }
          | lt_mark, Hit (loc, state), gt_mark ->
              let lt = rollback casn lt_mark lt
              and gt = rollback casn gt_mark gt in
              CASN { loc; state; lt; gt; awaiters = [] })

  type 'x snap = cass

  let snapshot ~xt = xt.cass
  let rollback ~xt snap = xt.cass <- rollback xt.casn snap xt.cass

  let rec first ~xt tx = function
    | [] -> tx ~xt
    | tx' :: txs -> (
        match tx ~xt with
        | value -> value
        | exception Retry.Later -> first ~xt tx' txs)

  let first ~xt = function
    | [] -> Retry.later ()
    | tx :: txs -> first ~xt tx txs

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
        | CASN { loc; state; lt = NIL; gt = NIL; _ } ->
            if is_cmp xt.casn state then Action.run xt.post_commit result
            else
              let before = state.before in
              state.before <- state.after;
              state.casn <- casn_after;
              (* Fenceless is safe inside transactions as each log update has a fence. *)
              let state_old = fenceless_get (as_atomic loc) in
              if cas_with_state loc before state state_old then
                Action.run xt.post_commit result
              else commit (Backoff.once backoff) mode (reset_quick xt) tx
        | cass -> (
            match determine_for_owner xt.casn cass with
            | true -> Action.run xt.post_commit result
            | false -> commit (Backoff.once backoff) mode (reset mode xt) tx
            | exception Mode.Interference ->
                commit (Backoff.once backoff) Mode.lock_free
                  (reset Mode.lock_free xt) tx))
    | exception Retry.Invalid ->
        commit (Backoff.once backoff) mode (reset_quick xt) tx
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
