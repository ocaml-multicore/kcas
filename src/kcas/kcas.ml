(*
 * Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
 *)

(** Work around CSE bug in OCaml 5-5.1. *)
let[@inline] atomic_get x =
  Atomic.get ((* Prevents CSE *) Sys.opaque_identity x)

(* NOTE: You can adjust comment blocks below to select whether or not to use
   fenceless operations where it is safe to do so.  Fenceless operations have
   been seen to provide significant performance improvements on ARM (Apple
   M1). *)

(**)
external fenceless_get : 'a Atomic.t -> 'a = "%field0"
external fenceless_set : 'a Atomic.t -> 'a -> unit = "%setfield0"

let[@inline] fenceless_get x =
  fenceless_get ((* Prevents CSE *) Sys.opaque_identity x)
(**)
(*
let fenceless_get = atomic_get
let fenceless_set = Atomic.set
*)

module Timeout = struct
  exception Timeout

  let[@inline never] timeout () = raise Timeout

  type t = Unset | Elapsed | Call of (unit -> unit)

  let unset = Atomic.make Unset

  (* Fenceless operations are safe here as the timeout state is not not visible
     outside of the library and we don't always need the latest value and, when
     we do, there is a fence after. *)

  let[@inline] check state = if fenceless_get state == Elapsed then timeout ()

  let set seconds state =
    Domain_local_timeout.set_timeoutf seconds @@ fun () ->
    match Atomic.exchange state Elapsed with
    | Call release_or_cancel -> release_or_cancel ()
    | Unset | Elapsed -> ()

  let[@inline never] alloc_opt = function
    | None -> unset
    | Some seconds ->
        let state = Atomic.make Unset in
        let cancel = set seconds state in
        fenceless_set state @@ Call cancel;
        state

  let[@inline] alloc_opt seconds =
    if seconds == None then unset else alloc_opt seconds

  let[@inline never] set_opt state = function
    | None -> ()
    | Some seconds ->
        let cancel = set seconds state in
        fenceless_set state @@ Call cancel

  let[@inline] set_opt state seconds =
    if seconds != None then set_opt state seconds

  let[@inline never] await state release =
    match fenceless_get state with
    | Call _ as alive ->
        if Atomic.compare_and_set state alive (Call release) then alive
        else timeout ()
    | Unset | Elapsed -> timeout ()

  let[@inline] await state release =
    let alive = fenceless_get state in
    if alive == Unset then Unset else await state release

  let[@inline never] unawait state alive =
    match fenceless_get state with
    | Call _ as await ->
        if not (Atomic.compare_and_set state await alive) then timeout ()
    | Unset | Elapsed -> timeout ()

  let[@inline] unawait state alive = if alive != Unset then unawait state alive

  let[@inline never] cancel_alive alive =
    match alive with Call cancel -> cancel () | Unset | Elapsed -> ()

  let[@inline] cancel_alive alive = if alive != Unset then cancel_alive alive
  let[@inline] cancel state = cancel_alive (fenceless_get state)
end

module Id = struct
  let neg_id = Atomic.make (-1)
  let[@inline] neg_ids n = Atomic.fetch_and_add neg_id (-n)
  let[@inline] neg_id () = neg_ids 1
  let nat_id = Atomic.make Int.max_int
  let[@inline] nat_ids n = Atomic.fetch_and_add nat_id (-n)
  let[@inline] nat_id () = nat_ids 1
end

module Action : sig
  type t

  val noop : t
  val append : (unit -> unit) -> t -> t

  val run : t -> 'a -> 'a
  (** Always call this last as user code may raise. *)
end = struct
  type t = unit -> unit

  let noop = Fun.id

  let[@inline] append action t =
    if t == noop then action else fun x -> action (t x)

  let[@inline] run t x =
    t ();
    x
end

type awaiter = unit -> unit

let[@inline] resume_awaiter awaiter = awaiter ()

let[@inline] resume_awaiters = function
  | [] -> ()
  | [ awaiter ] -> resume_awaiter awaiter
  | awaiters -> List.iter resume_awaiter awaiters

type 'a state =
  | State : {
      mutable before : 'a;
      mutable after : 'a;
      mutable casn : [ `Determined | `Undetermined ] casn;
      awaiters : awaiter list;
    }
      -> 'a state

and cass =
  | Bef : cass  (** [Bef] must be consistently used as the "Nil" value. *)
  | Aft : cass
  | Cas : {
      loc : 'a loc;
      state : 'a state;
      lt : cass;
      gt : cass;
      mutable awaiters : awaiter list;
    }
      -> cass

and _ casn =
  | Before : [> `Determined ] casn
  | After : [> `Determined ] casn
  | Undetermined : { mutable cass : cass } -> [> `Undetermined ] casn

(* NOTE: You can adjust comment blocks below to select whether or not to use an
   unsafe cast to avoid a level of indirection due to [Atomic.t] and reduce the
   size of a location by two words (or more when padded).  This has been seen
   to provide significant performance improvements. *)

(**)
and 'a loc = { mutable _state : 'a state; id : int }

external as_atomic : 'a loc -> 'a state Atomic.t = "%identity"

let[@inline] make_loc padded state id =
  let record = { _state = state; id } in
  if padded then Multicore_magic.copy_as_padded record else record
(**)

(*
and 'a loc = { state : 'a state Atomic.t; id : int }

let[@inline] as_atomic loc = loc.state

let[@inline] make_loc padded state id =
  let atomic = Atomic.make state in
  let state =
    if padded then Multicore_magic.copy_as_padded atomic else atomic
  in
  let record = { state; id } in
  if padded then Multicore_magic.copy_as_padded record else record
*)

external casn_as_atomic : [ `Undetermined ] casn -> cass Atomic.t = "%identity"

external casn_upcast :
  [ `Undetermined ] casn -> [ `Determined | `Undetermined ] casn = "%identity"

let[@inline] is_node cass =
  (* This assumes [Bef] is consistently used as the "Nil" value. *)
  cass != Bef

let[@inline] is_cmp casn (State state) = state.casn != casn_upcast casn
let[@inline] is_cas casn (State state) = state.casn == casn_upcast casn

let[@inline] is_determined = function
  | (Undetermined _ as casn : [ `Undetermined ] casn) -> begin
      match fenceless_get (casn_as_atomic casn) with
      | Cas _ -> false
      | Aft | Bef -> true
    end

module Mode = struct
  type t = cass

  let lock_free = Aft
  let obstruction_free = Bef

  exception Interference
end

let rec release_after casn = function
  | Bef | Aft -> true
  | Cas cas_r ->
      let lt = cas_r.lt in
      if is_node lt then release_after casn lt |> ignore;
      let (State state_r as state) = cas_r.state in
      if is_cas casn state then begin
        state_r.casn <- After;
        state_r.before <- Obj.magic ();
        resume_awaiters cas_r.awaiters
      end;
      release_after casn cas_r.gt

let rec release_before casn = function
  | Bef | Aft -> false
  | Cas cas_r ->
      let lt = cas_r.lt in
      if is_node lt then release_before casn lt |> ignore;
      let (State state_r as state) = cas_r.state in
      if is_cas casn state then begin
        state_r.casn <- Before;
        state_r.after <- Obj.magic ();
        resume_awaiters cas_r.awaiters
      end;
      release_before casn cas_r.gt

let release casn cass status =
  if status == Aft then release_after casn cass else release_before casn cass

let rec verify casn = function
  | Bef | Aft -> Aft
  | Cas cas_r ->
      let lt = cas_r.lt in
      if is_node lt then
        let status = verify casn lt in
        if status == Aft then
          let state = cas_r.state in
          (* Fenceless is safe as [finish] has a fence after. *)
          if is_cmp casn state && fenceless_get (as_atomic cas_r.loc) != state
          then Bef
          else verify casn cas_r.gt
        else status
      else
        (* Fenceless is safe as [finish] has a fence after. *)
        let state = cas_r.state in
        if is_cmp casn state && fenceless_get (as_atomic cas_r.loc) != state
        then Bef
        else verify casn cas_r.gt

let finish casn cass undetermined status =
  if Atomic.compare_and_set (casn_as_atomic casn) undetermined status then
    release casn cass status
  else
    (* Fenceless is safe as we have a fence above. *)
    fenceless_get (casn_as_atomic casn) == Aft

let a_cmp = 1
let a_cas = 2
let a_cmp_followed_by_a_cas = 4

let rec determine (casn : [ `Undetermined ] casn) status = function
  | Bef | Aft -> status
  | Cas r as eq ->
      let lt = r.lt in
      let status = if is_node lt then determine casn status lt else status in
      if status < 0 then status
      else
        let loc = r.loc in
        let (State current_r as current) = atomic_get (as_atomic loc) in
        let (State state_r as state) = r.state in
        if state == current then begin
          let a_cas_or_a_cmp = 1 + Bool.to_int (is_cas casn state) in
          let a_cmp_followed_by_a_cas = a_cas_or_a_cmp * 2 land (status * 4) in
          if is_determined casn then raise_notrace Exit;
          determine casn
            (status lor a_cas_or_a_cmp lor a_cmp_followed_by_a_cas)
            r.gt
        end
        else
          let matches_expected () =
            match current_r.casn with
            | Before -> state_r.before == current_r.before
            | After -> state_r.before == current_r.after
            | Undetermined _ as casn ->
                if is_after casn then state_r.before == current_r.after
                else state_r.before == current_r.before
          in
          if is_cas casn state && matches_expected () then begin
            if is_determined casn then raise_notrace Exit;
            (* We now know that the operation wasn't finished when we read
               [current], but it is possible that the [loc]ation has been
               updated since then by some other domain helping us (or even by
               some later operation).  If so, then the [compare_and_set] below
               fails.  Copying the awaiters from [current] is safe in either
               case, because we know that we have the [current] state that our
               operation is interested in.  By doing the copying here, we at
               most duplicate work already done by some other domain.  However,
               it is necessary to do the copy before the [compare_and_set],
               because afterwards is too late as some other domain might finish
               the operation after the [compare_and_set] and miss the
               awaiters. *)
            begin
              match current_r.awaiters with
              | [] -> ()
              | awaiters -> r.awaiters <- awaiters
            end;
            if Atomic.compare_and_set (as_atomic loc) current state then
              let a_cmp_followed_by_a_cas = a_cas * 2 land (status * 4) in
              determine casn (status lor a_cas lor a_cmp_followed_by_a_cas) r.gt
            else determine casn status eq
          end
          else -1

and is_after = function
  | (Undetermined _ as casn : [ `Undetermined ] casn) -> begin
      (* Fenceless at most gives old [Undetermined] and causes extra work. *)
      match fenceless_get (casn_as_atomic casn) with
      | Cas _ as cass -> begin
          match determine casn 0 cass with
          | status ->
              finish casn cass cass
                (if a_cmp_followed_by_a_cas < status then verify casn cass
                 else if 0 <= status then Aft
                 else Bef)
          | exception Exit ->
              (* Fenceless is safe as there was a fence before. *)
              fenceless_get (casn_as_atomic casn) == Aft
        end
      | Bef -> false
      | Aft -> true
    end

let[@inline] determine_for_owner casn cass =
  (* Fenceless is safe as [casn] is private at this point. *)
  fenceless_set (casn_as_atomic casn) cass;
  (* The end result is a cyclic data structure, which is why we cannot
     initialize the [casn] atomic directly. *)
  match determine casn 0 cass with
  | status ->
      if a_cmp_followed_by_a_cas < status then
        (* We only want to [raise Interference] in case it is the verify step
           that fails.  The idea is that in [lock_free] mode the attempt might
           have succeeded as the compared locations would have been set in
           [lock_free] mode preventing interference.  If failure happens before
           the verify step then the [lock_free] mode would have likely also
           failed. *)
        finish casn cass cass (verify casn cass)
        || raise_notrace Mode.Interference
      else
        a_cmp = status
        || finish casn cass cass (if 0 <= status then Aft else Bef)
  | exception Exit ->
      (* Fenceless is safe as there was a fence before. *)
      fenceless_get (casn_as_atomic casn) == Aft

let[@inline never] impossible () = failwith "impossible"
let[@inline never] overlap () = failwith "kcas: location overlap"
let[@inline never] invalid_retry () = failwith "kcas: invalid use of retry"

type splay = Miss : splay | Hit : 'a loc * 'a state -> splay

let[@inline] make_casn loc state lt gt =
  Cas { loc; state; lt; gt; awaiters = [] }

let rec splay ~hit_parent x = function
  | Bef | Aft -> (Bef, Miss, Bef)
  | Cas { loc = a; state = s; lt = l; gt = r; _ } as t ->
      if x < a.id && ((not hit_parent) || is_node l) then
        match l with
        | Bef | Aft -> (Bef, Miss, t)
        | Cas { loc = pa; state = ps; lt = ll; gt = lr; _ } ->
            if x < pa.id && ((not hit_parent) || is_node ll) then
              let lll, n, llr = splay ~hit_parent x ll in
              (lll, n, make_casn pa ps llr (make_casn a s lr r))
            else if pa.id < x && ((not hit_parent) || is_node lr) then
              let lrl, n, lrr = splay ~hit_parent x lr in
              (make_casn pa ps ll lrl, n, make_casn a s lrr r)
            else (ll, Hit (pa, ps), make_casn a s lr r)
      else if a.id < x && ((not hit_parent) || is_node r) then
        match r with
        | Bef | Aft -> (t, Miss, Bef)
        | Cas { loc = pa; state = ps; lt = rl; gt = rr; _ } ->
            if x < pa.id && ((not hit_parent) || is_node rl) then
              let rll, n, rlr = splay ~hit_parent x rl in
              (make_casn a s l rll, n, make_casn pa ps rlr rr)
            else if pa.id < x && ((not hit_parent) || is_node rr) then
              let rrl, n, rrr = splay ~hit_parent x rr in
              (make_casn pa ps (make_casn a s l rl) rrl, n, rrr)
            else (make_casn a s l rl, Hit (pa, ps), rr)
      else (l, Hit (a, s), r)

let[@inline] new_state after =
  State { before = Obj.magic (); after; casn = After; awaiters = [] }

let[@inline] eval (State state_r) =
  match state_r.casn with
  | Before -> state_r.before
  | After -> state_r.after
  | Undetermined _ as casn ->
      if is_after casn then state_r.after else state_r.before

module Retry = struct
  exception Later

  let[@inline never] later () = raise_notrace Later
  let[@inline] unless condition = if not condition then later ()

  exception Invalid

  let[@inline never] invalid () = raise_notrace Invalid
end

let add_awaiter loc before awaiter =
  (* Fenceless is safe as we have fence after. *)
  let (State state_old_r as state_old) = fenceless_get (as_atomic loc) in
  let state_new =
    let awaiters = awaiter :: state_old_r.awaiters in
    State { before = Obj.magic (); after = before; casn = After; awaiters }
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
  let (State state_old_r as state_old) = fenceless_get (as_atomic loc) in
  if before == eval state_old then
    let removed = ref true in
    let awaiters = remove_first awaiter removed state_old_r.awaiters in
    if !removed then
      let state_new =
        State { before = Obj.magic (); after = before; casn = After; awaiters }
      in
      if not (Atomic.compare_and_set (as_atomic loc) state_old state_new) then
        remove_awaiter loc before awaiter

let block timeout loc before =
  let t = Domain_local_await.prepare_for_await () in
  let alive = Timeout.await timeout t.release in
  if add_awaiter loc before t.release then begin
    try t.await ()
    with cancellation_exn ->
      remove_awaiter loc before t.release;
      Timeout.cancel_alive alive;
      raise cancellation_exn
  end;
  Timeout.unawait timeout alive

let rec update_no_alloc timeout backoff loc (State state_r as state) f =
  (* Fenceless is safe as we have had a fence before if needed and there is a fence after. *)
  let (State state_old_r as state_old) = fenceless_get (as_atomic loc) in
  let before = eval state_old in
  match f before with
  | after ->
      state_r.after <- after;
      if before == after then begin
        Timeout.cancel timeout;
        before
      end
      else if Atomic.compare_and_set (as_atomic loc) state_old state then begin
        resume_awaiters state_old_r.awaiters;
        Timeout.cancel timeout;
        before
      end
      else update_no_alloc timeout (Backoff.once backoff) loc state f
  | exception Retry.Later ->
      block timeout loc before;
      update_no_alloc timeout backoff loc state f
  | exception exn ->
      Timeout.cancel timeout;
      raise exn

let update_with_state timeout backoff loc f (State state_old_r as state_old) =
  let before = eval state_old in
  match f before with
  | after ->
      if before == after then begin
        Timeout.cancel timeout;
        before
      end
      else
        let state = new_state after in
        if Atomic.compare_and_set (as_atomic loc) state_old state then begin
          resume_awaiters state_old_r.awaiters;
          Timeout.cancel timeout;
          before
        end
        else update_no_alloc timeout (Backoff.once backoff) loc state f
  | exception Retry.Later ->
      let state = new_state before in
      block timeout loc before;
      update_no_alloc timeout backoff loc state f
  | exception exn ->
      Timeout.cancel timeout;
      raise exn

let rec exchange_no_alloc backoff loc (State state_r as state) =
  let (State state_old_r as state_old) = atomic_get (as_atomic loc) in
  let before = eval state_old in
  if before == state_r.after then before
  else if Atomic.compare_and_set (as_atomic loc) state_old state then begin
    resume_awaiters state_old_r.awaiters;
    before
  end
  else exchange_no_alloc (Backoff.once backoff) loc state

let[@inline] is_obstruction_free casn loc =
  (* Fenceless is safe as we are accessing a private location. *)
  fenceless_get (casn_as_atomic casn) == Mode.obstruction_free && 0 <= loc.id

let[@inline] rec cas_with_state loc before (State state_r as state)
    (State state_old_r as state_old) =
  before == eval state_old
  && (before == state_r.after
     ||
     if Atomic.compare_and_set (as_atomic loc) state_old state then begin
       resume_awaiters state_old_r.awaiters;
       true
     end
     else
       (* We must retry, because compare is by value rather than by state.  In
          other words, we should not fail spuriously due to some other thread
          having installed or removed a waiter.

          Fenceless is safe as there was a fence before. *)
       cas_with_state loc before state (fenceless_get (as_atomic loc)))

let inc x = x + 1
let dec x = x - 1

module Loc = struct
  type 'a t = 'a loc

  let make ?(padded = false) ?(mode = Mode.obstruction_free) after =
    let state = new_state after
    and id =
      if mode == Mode.obstruction_free then Id.nat_id () else Id.neg_id ()
    in
    make_loc padded state id

  let make_contended ?mode after = make ~padded:true ?mode after

  let make_array ?(padded = false) ?(mode = Mode.obstruction_free) n after =
    assert (0 <= n);
    let state = new_state after
    and id =
      (if mode == Mode.obstruction_free then Id.nat_ids n else Id.neg_ids n)
      - (n - 1)
    in
    Array.init n @@ fun i -> make_loc padded state (id + i)

  let[@inline] get_id loc = loc.id
  let get loc = eval (atomic_get (as_atomic loc))

  let rec get_as timeout f loc state =
    let before = eval state in
    match f before with
    | value ->
        Timeout.cancel timeout;
        value
    | exception Retry.Later ->
        block timeout loc before;
        (* Fenceless is safe as there was already a fence before. *)
        get_as timeout f loc (fenceless_get (as_atomic loc))
    | exception exn ->
        Timeout.cancel timeout;
        raise exn

  let[@inline] get_as ?timeoutf f loc =
    get_as (Timeout.alloc_opt timeoutf) f loc (atomic_get (as_atomic loc))

  let[@inline] get_mode loc =
    if loc.id < 0 then Mode.lock_free else Mode.obstruction_free

  let compare_and_set loc before after =
    let state = new_state after in
    let state_old = atomic_get (as_atomic loc) in
    cas_with_state loc before state state_old

  let fenceless_update ?timeoutf ?(backoff = Backoff.default) loc f =
    let timeout = Timeout.alloc_opt timeoutf in
    update_with_state timeout backoff loc f (fenceless_get (as_atomic loc))

  let[@inline] fenceless_modify ?timeoutf ?backoff loc f =
    fenceless_update ?timeoutf ?backoff loc f |> ignore

  let update ?timeoutf ?(backoff = Backoff.default) loc f =
    let timeout = Timeout.alloc_opt timeoutf in
    update_with_state timeout backoff loc f (atomic_get (as_atomic loc))

  let[@inline] modify ?timeoutf ?backoff loc f =
    update ?timeoutf ?backoff loc f |> ignore

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
    let (State state_r) = atomic_get (as_atomic loc) in
    state_r.awaiters != []

  let fenceless_get loc = eval (fenceless_get (as_atomic loc))
end

let[@inline] insert cass loc state =
  let x = loc.id in
  match cass with
  | Cas { loc = a; lt = Bef | Aft; _ } when x < a.id ->
      Cas { loc; state; lt = Bef; gt = cass; awaiters = [] }
  | Cas { loc = a; gt = Bef | Aft; _ } when a.id < x ->
      Cas { loc; state; lt = cass; gt = Bef; awaiters = [] }
  | _ -> begin
      match splay ~hit_parent:false x cass with
      | _, Hit _, _ -> overlap ()
      | lt, Miss, gt -> Cas { loc; state; lt; gt; awaiters = [] }
    end

module Op = struct
  type t = CAS : 'a Loc.t * 'a * 'a -> t

  let[@inline] make_cas loc before after = CAS (loc, before, after)
  let[@inline] make_cmp loc expected = CAS (loc, expected, expected)

  let[@inline] is_on_loc op loc =
    match op with CAS (loc', _, _) -> Obj.magic loc' == loc

  let[@inline] get_id = function CAS (loc, _, _) -> loc.id

  let atomic = function
    | CAS (loc, before, after) ->
        if before == after then Loc.get loc == before
        else Loc.compare_and_set loc before after

  let atomically ?(mode = Mode.lock_free) = function
    | [] -> true
    | [ op ] -> atomic op
    | first :: rest ->
        let casn = Undetermined { cass = mode } in
        let rec run cass = function
          | [] -> determine_for_owner casn cass
          | CAS (loc, before, after) :: rest ->
              if before == after && is_obstruction_free casn loc then
                (* Fenceless is safe as there are fences in [determine]. *)
                let state = fenceless_get (as_atomic loc) in
                before == eval state && run (insert cass loc state) rest
              else
                run
                  (insert cass loc
                     (let casn = casn_upcast casn in
                      State { before; after; casn; awaiters = [] }))
                  rest
        in
        let (CAS (loc, before, after)) = first in
        if before == after && is_obstruction_free casn loc then
          (* Fenceless is safe as there are fences in [determine]. *)
          let state = fenceless_get (as_atomic loc) in
          before == eval state
          && run (Cas { loc; state; lt = Bef; gt = Bef; awaiters = [] }) rest
        else
          let state =
            State { before; after; casn = casn_upcast casn; awaiters = [] }
          in
          run (Cas { loc; state; lt = Bef; gt = Bef; awaiters = [] }) rest
end

module Xt = struct
  (* NOTE: You can adjust comment blocks below to select whether or not to use
     an unsafe cast to avoid a level of indirection due to [Atomic.t]. *)

  (**)
  type 'x t = {
    mutable _timeout : Timeout.t;
    mutable casn : [ `Undetermined ] casn;
    mutable cass : cass;
    mutable validate_counter : int;
    mutable post_commit : Action.t;
  }

  let[@inline] timeout_unset () = Timeout.Unset

  external timeout_as_atomic : 'x t -> Timeout.t Atomic.t = "%identity"
  (**)

  (*
  type 'x t = {
    mutable _timeout : Timeout.t Atomic.t;
    mutable casn : casn;
    mutable cass : cass;
    mutable validate_counter : int;
    mutable post_commit : Action.t;
  }

  let[@inline] timeout_unset () = Atomic.make Timeout.Unset
  let[@inline] timeout_as_atomic r = r._timeout
  *)

  let[@inline] validate_one casn loc (State state_r as state) =
    let before = if is_cmp casn state then eval state else state_r.before in
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    if before != eval (fenceless_get (as_atomic loc)) then Retry.invalid ()

  let rec validate_all casn = function
    | Bef | Aft -> ()
    | Cas r ->
        let lt = r.lt in
        if is_node lt then validate_all casn lt;
        validate_one casn r.loc r.state;
        validate_all casn r.gt

  let[@inline] maybe_validate_log xt =
    let c0 = xt.validate_counter in
    let c1 = c0 + 1 in
    xt.validate_counter <- c1;
    (* Validate whenever counter reaches next power of 2. *)
    if c0 land c1 = 0 then begin
      Timeout.check (timeout_as_atomic xt);
      validate_all xt.casn xt.cass
    end

  let[@inline] update_new loc f xt lt gt =
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    let state = fenceless_get (as_atomic loc) in
    let before = eval state in
    match f before with
    | after ->
        let state =
          if before == after && is_obstruction_free xt.casn loc then state
          else
            State { before; after; casn = casn_upcast xt.casn; awaiters = [] }
        in
        xt.cass <- Cas { loc; state; lt; gt; awaiters = [] };
        before
    | exception exn ->
        xt.cass <- Cas { loc; state; lt; gt; awaiters = [] };
        raise exn

  let[@inline] update_top loc f xt state' lt gt =
    let (State state_r as state) = Obj.magic state' in
    if is_cmp xt.casn state then begin
      let before = eval state in
      let after = f before in
      let state =
        if before == after then state
        else State { before; after; casn = casn_upcast xt.casn; awaiters = [] }
      in
      xt.cass <- Cas { loc; state; lt; gt; awaiters = [] };
      before
    end
    else
      let current = state_r.after in
      let state = State { state_r with after = f current } in
      xt.cass <- Cas { loc; state; lt; gt; awaiters = [] };
      current

  let[@inline] unsafe_update ~xt loc f =
    maybe_validate_log xt;
    let x = loc.id in
    match xt.cass with
    | Bef | Aft -> update_new loc f xt Bef Bef
    | Cas { loc = a; lt = Bef | Aft; _ } as cass when x < a.id ->
        update_new loc f xt Bef cass
    | Cas { loc = a; gt = Bef | Aft; _ } as cass when a.id < x ->
        update_new loc f xt cass Bef
    | Cas { loc = a; state; lt; gt; _ } when Obj.magic a == loc ->
        update_top loc f xt state lt gt
    | cass -> begin
        match splay ~hit_parent:false x cass with
        | l, Miss, r -> update_new loc f xt l r
        | l, Hit (_loc', state'), r -> update_top loc f xt state' l r
      end

  let[@inline] protect xt f x =
    let cass = xt.cass in
    let y = f x in
    assert (xt.cass == cass);
    y

  let get ~xt loc = unsafe_update ~xt loc Fun.id
  let set ~xt loc after = unsafe_update ~xt loc (fun _ -> after) |> ignore
  let modify ~xt loc f = unsafe_update ~xt loc (protect xt f) |> ignore

  let compare_and_swap ~xt loc before after =
    unsafe_update ~xt loc (fun actual ->
        if actual == before then after else actual)

  let compare_and_set ~xt loc before after =
    compare_and_swap ~xt loc before after == before

  let exchange ~xt loc after = unsafe_update ~xt loc (fun _ -> after)
  let fetch_and_add ~xt loc n = unsafe_update ~xt loc (( + ) n)
  let incr ~xt loc = unsafe_update ~xt loc inc |> ignore
  let decr ~xt loc = unsafe_update ~xt loc dec |> ignore
  let update ~xt loc f = unsafe_update ~xt loc (protect xt f)
  let swap ~xt l1 l2 = set ~xt l1 @@ exchange ~xt l2 @@ get ~xt l1
  let unsafe_modify ~xt loc f = unsafe_update ~xt loc f |> ignore
  let unsafe_update ~xt loc f = unsafe_update ~xt loc f

  let[@inline] to_blocking ~xt tx =
    match tx ~xt with None -> Retry.later () | Some value -> value

  let[@inline] to_nonblocking ~xt tx =
    match tx ~xt with value -> Some value | exception Retry.Later -> None

  let post_commit ~xt action =
    xt.post_commit <- Action.append action xt.post_commit

  let validate ~xt loc =
    let x = loc.id in
    match xt.cass with
    | Bef | Aft -> ()
    | Cas { loc = a; lt = Bef | Aft; _ } when x < a.id -> ()
    | Cas { loc = a; gt = Bef | Aft; _ } when a.id < x -> ()
    | Cas { loc = a; state; _ } when Obj.magic a == loc ->
        validate_one xt.casn a state
    | cass -> begin
        match splay ~hit_parent:true x cass with
        | lt, Hit (a, state), gt ->
            xt.cass <- Cas { loc = a; state; lt; gt; awaiters = [] };
            if Obj.magic a == loc then validate_one xt.casn a state
        | _, Miss, _ -> impossible ()
      end

  let is_in_log ~xt loc =
    let x = loc.id in
    match xt.cass with
    | Bef | Aft -> false
    | Cas { loc = a; lt = Bef | Aft; _ } when x < a.id -> false
    | Cas { loc = a; gt = Bef | Aft; _ } when a.id < x -> false
    | Cas { loc = a; _ } when Obj.magic a == loc -> true
    | cass -> begin
        match splay ~hit_parent:true x cass with
        | lt, Hit (a, state), gt ->
            xt.cass <- Cas { loc = a; state; lt; gt; awaiters = [] };
            Obj.magic a == loc
        | _, Miss, _ -> impossible ()
      end

  let rec rollback casn cass_snap cass =
    if cass_snap == cass then cass
    else
      match cass with
      | (Bef | Aft) as nil -> nil
      | Cas r -> begin
          let loc = r.loc in
          match splay ~hit_parent:false loc.id cass_snap with
          | lt_mark, Miss, gt_mark ->
              let lt = rollback casn lt_mark r.lt
              and gt = rollback casn gt_mark r.gt in
              let state =
                let (State state_r as state) = r.state in
                if is_cmp casn state then state
                else
                  (* Fenceless is safe inside transactions as each log update has a fence. *)
                  let current = fenceless_get (as_atomic loc) in
                  if state_r.before != eval current then Retry.invalid ()
                  else current
              in
              Cas { loc; state; lt; gt; awaiters = [] }
          | lt_mark, Hit (loc, state), gt_mark ->
              let lt = rollback casn lt_mark r.lt
              and gt = rollback casn gt_mark r.gt in
              Cas { loc; state; lt; gt; awaiters = [] }
        end

  type 'x snap = cass * Action.t

  let snapshot ~xt = (xt.cass, xt.post_commit)

  let rollback ~xt (snap, post_commit) =
    xt.cass <- rollback xt.casn snap xt.cass;
    xt.post_commit <- post_commit

  let rec first ~xt tx = function
    | [] -> tx ~xt
    | tx' :: txs -> begin
        match tx ~xt with
        | value -> value
        | exception Retry.Later -> first ~xt tx' txs
      end

  let first ~xt = function
    | [] -> Retry.later ()
    | tx :: txs -> first ~xt tx txs

  type 'a tx = { tx : 'x. xt:'x t -> 'a } [@@unboxed]

  let[@inline] call ~xt { tx } = tx ~xt

  let rec add_awaiters awaiter casn = function
    | (Bef | Aft) as nil -> nil
    | Cas r as stop -> begin
        let lt = r.lt in
        match if is_node lt then add_awaiters awaiter casn lt else lt with
        | Bef | Aft ->
            if
              add_awaiter r.loc
                (let (State state_r as state) = r.state in
                 if is_cmp casn state then eval state else state_r.before)
                awaiter
            then add_awaiters awaiter casn r.gt
            else stop
        | Cas _ as stop -> stop
      end

  let rec remove_awaiters awaiter casn stop = function
    | Bef | Aft -> ()
    | Cas r as current ->
        let lt = r.lt in
        if is_node lt then remove_awaiters awaiter casn stop lt;
        if current != stop then begin
          remove_awaiter r.loc
            (let (State state_r as state) = r.state in
             if is_cmp casn state then eval state else state_r.before)
            awaiter;
          remove_awaiters awaiter casn stop r.gt
        end

  let initial_validate_period = 16

  let[@inline] reset_quick xt =
    xt.cass <- Bef;
    xt.validate_counter <- initial_validate_period;
    xt.post_commit <- Action.noop;
    xt

  let reset mode xt =
    xt.casn <- Undetermined { cass = mode };
    reset_quick xt

  let rec commit backoff mode xt tx =
    match tx ~xt with
    | result -> begin
        match xt.cass with
        | Bef | Aft ->
            Timeout.cancel (timeout_as_atomic xt);
            Action.run xt.post_commit result
        | Cas
            {
              loc;
              state = State state_r as state;
              lt = Bef | Aft;
              gt = Bef | Aft;
              _;
            } ->
            if is_cmp xt.casn state then begin
              Timeout.cancel (timeout_as_atomic xt);
              Action.run xt.post_commit result
            end
            else
              let before = state_r.before in
              state_r.before <- Obj.magic ();
              state_r.casn <- After;
              (* Fenceless is safe inside transactions as each log update has a fence. *)
              let state_old = fenceless_get (as_atomic loc) in
              if cas_with_state loc before state state_old then begin
                Timeout.cancel (timeout_as_atomic xt);
                Action.run xt.post_commit result
              end
              else commit (Backoff.once backoff) mode (reset_quick xt) tx
        | cass -> begin
            match determine_for_owner xt.casn cass with
            | true ->
                Timeout.cancel (timeout_as_atomic xt);
                Action.run xt.post_commit result
            | false -> commit (Backoff.once backoff) mode (reset mode xt) tx
            | exception Mode.Interference ->
                commit (Backoff.once backoff) Mode.lock_free
                  (reset Mode.lock_free xt) tx
          end
      end
    | exception Retry.Invalid ->
        Timeout.check (timeout_as_atomic xt);
        commit (Backoff.once backoff) mode (reset_quick xt) tx
    | exception Retry.Later -> begin
        if xt.cass == Bef then invalid_retry ();
        let t = Domain_local_await.prepare_for_await () in
        let alive = Timeout.await (timeout_as_atomic xt) t.release in
        match add_awaiters t.release xt.casn xt.cass with
        | Bef | Aft -> begin
            match t.await () with
            | () ->
                remove_awaiters t.release xt.casn Bef xt.cass;
                Timeout.unawait (timeout_as_atomic xt) alive;
                commit (Backoff.reset backoff) mode (reset_quick xt) tx
            | exception cancellation_exn ->
                remove_awaiters t.release xt.casn Bef xt.cass;
                Timeout.cancel_alive alive;
                raise cancellation_exn
          end
        | Cas _ as stop ->
            remove_awaiters t.release xt.casn stop xt.cass;
            Timeout.unawait (timeout_as_atomic xt) alive;
            commit (Backoff.once backoff) mode (reset_quick xt) tx
      end
    | exception exn ->
        Timeout.cancel (timeout_as_atomic xt);
        raise exn

  let[@inline] commit ?timeoutf ?(backoff = Backoff.default)
      ?(mode = Mode.obstruction_free) tx =
    let casn = Undetermined { cass = mode }
    and cass = Bef
    and validate_counter = initial_validate_period
    and post_commit = Action.noop in
    let xt =
      { _timeout = timeout_unset (); casn; cass; validate_counter; post_commit }
    in
    Timeout.set_opt (timeout_as_atomic xt) timeoutf;
    commit backoff mode xt tx.tx
end
