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

type 'a state = {
  mutable before : 'a;
  mutable after : 'a;
  mutable which : which;
  awaiters : awaiter list;
}

(** Tagged GADT for representing both the state of MCAS operations and of the
    transaction log or splay [tree].  Different subsets of this GADT are used in
    different contexts.  See the [root], [tree], and [which] existentials. *)
and _ tdt =
  | Before : [> `Before ] tdt
      (** The result has been determined to be the [before] value.

          Keep this first (i.e. value [0] or [false]) for best performance. *)
  | After : [> `After ] tdt
      (** The result has been determined to be the [after] value.

          Keep this second (i.e. value [1] or [true]) for best performance. *)
  | Undetermined : { mutable root : root } -> [> `Undetermined ] tdt
      (** The result might not yet have been determined.  The [root] either says
          which it is or points to the root of the transaction log or [tree]. *)
  | Leaf : [> `Leaf ] tdt  (** Leaf node in the transaction log or [tree]. *)
  | Node : {
      loc : 'a loc;
      state : 'a state;
      lt : tree;
      gt : tree;
      mutable awaiters : awaiter list;
    }
      -> [> `Node ] tdt
      (** Branch node in the transaction log or [tree] that specifies a single
          [CAS] or [CMP] operation. *)

and root = R : [< `Before | `After | `Node ] tdt -> root [@@unboxed]
and tree = T : [< `Leaf | `Node ] tdt -> tree [@@unboxed]
and which = W : [< `Before | `After | `Undetermined ] tdt -> which [@@unboxed]

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

external root_as_atomic : [< `Undetermined ] tdt -> root Atomic.t = "%identity"

let[@inline] is_node tree = tree != T Leaf
let[@inline] is_cmp which state = state.which != W which
let[@inline] is_cas which state = state.which == W which

let[@inline] is_determined = function
  | (Undetermined _ as which : [< `Undetermined ] tdt) -> begin
      match fenceless_get (root_as_atomic which) with
      | R (Node _) -> false
      | R After | R Before -> true
    end

module Mode = struct
  type t = [ `Before | `After ] tdt

  let lock_free = After
  let obstruction_free = Before
end

let[@inline] isnt_int x = not (Obj.is_int (Obj.repr x))

let rec release_after which = function
  | T Leaf -> true
  | T (Node node_r) ->
      if is_node node_r.lt then release_after which node_r.lt |> ignore;
      let state = node_r.state in
      if is_cas which state then begin
        state.which <- W After;
        if isnt_int state.before then state.before <- Obj.magic ();
        resume_awaiters node_r.awaiters
      end;
      release_after which node_r.gt

let rec release_before which = function
  | T Leaf -> false
  | T (Node node_r) ->
      if is_node node_r.lt then release_before which node_r.lt |> ignore;
      let state = node_r.state in
      if is_cas which state then begin
        state.which <- W Before;
        if isnt_int state.after then state.after <- Obj.magic ();
        resume_awaiters node_r.awaiters
      end;
      release_before which node_r.gt

let release which tree status =
  if status == After then release_after which tree
  else release_before which tree

let rec verify which = function
  | T Leaf -> After
  | T (Node node_r) ->
      if is_node node_r.lt then
        let status = verify which node_r.lt in
        if status == After then
          (* Fenceless is safe as [finish] has a fence after. *)
          if
            is_cmp which node_r.state
            && fenceless_get (as_atomic node_r.loc) != node_r.state
          then Before
          else verify which node_r.gt
        else status
      else if
        (* Fenceless is safe as [finish] has a fence after. *)
        is_cmp which node_r.state
        && fenceless_get (as_atomic node_r.loc) != node_r.state
      then Before
      else verify which node_r.gt

let finish which root status =
  if Atomic.compare_and_set (root_as_atomic which) (R root) (R status) then
    release which (T root) status
  else
    (* Fenceless is safe as we have a fence above. *)
    fenceless_get (root_as_atomic which) == R After

let a_cmp = 1
let a_cas = 2
let a_cmp_followed_by_a_cas = 4

let rec determine which status = function
  | T Leaf -> status
  | T (Node node_r) as eq ->
      let status =
        if is_node node_r.lt then determine which status node_r.lt else status
      in
      if status < 0 then status
      else
        let current = atomic_get (as_atomic node_r.loc) in
        let state = node_r.state in
        if state == current then begin
          let a_cas_or_a_cmp = 1 + Bool.to_int (is_cas which state) in
          let a_cmp_followed_by_a_cas = a_cas_or_a_cmp * 2 land (status * 4) in
          if is_determined which then raise_notrace Exit;
          determine which
            (status lor a_cas_or_a_cmp lor a_cmp_followed_by_a_cas)
            node_r.gt
        end
        else
          let matches_expected () =
            match current.which with
            | W Before -> state.before == current.before
            | W After -> state.before == current.after
            | W (Undetermined _ as which) ->
                if is_after which then state.before == current.after
                else state.before == current.before
          in
          if is_cas which state && matches_expected () then begin
            if is_determined which then raise_notrace Exit;
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
            if current.awaiters != [] then node_r.awaiters <- current.awaiters;
            if Atomic.compare_and_set (as_atomic node_r.loc) current state then
              let a_cmp_followed_by_a_cas = a_cas * 2 land (status * 4) in
              determine which
                (status lor a_cas lor a_cmp_followed_by_a_cas)
                node_r.gt
            else determine which status eq
          end
          else -1

and is_after = function
  | (Undetermined _ as which : [< `Undetermined ] tdt) -> begin
      (* Fenceless at most gives old [Undetermined] and causes extra work. *)
      match fenceless_get (root_as_atomic which) with
      | R (Node node_r) -> begin
          let root = Node node_r in
          match determine which 0 (T root) with
          | status ->
              finish which root
                (if a_cmp_followed_by_a_cas < status then verify which (T root)
                 else if 0 <= status then After
                 else Before)
          | exception Exit ->
              (* Fenceless is safe as there was a fence before. *)
              fenceless_get (root_as_atomic which) == R After
        end
      | R Before -> false
      | R After -> true
    end

let[@inline never] impossible () = failwith "impossible"
let[@inline never] invalid_retry () = failwith "kcas: invalid use of retry"

let[@inline] make_node loc state lt gt =
  T (Node { loc; state; lt; gt; awaiters = [] })

let rec splay ~hit_parent x = function
  | T Leaf -> (T Leaf, T Leaf, T Leaf)
  | T (Node { loc = a; state = s; lt = l; gt = r; _ }) as t ->
      if x < a.id && ((not hit_parent) || is_node l) then
        match l with
        | T Leaf -> (T Leaf, T Leaf, t)
        | T (Node { loc = pa; state = ps; lt = ll; gt = lr; _ }) ->
            if x < pa.id && ((not hit_parent) || is_node ll) then
              let lll, n, llr = splay ~hit_parent x ll in
              (lll, n, make_node pa ps llr (make_node a s lr r))
            else if pa.id < x && ((not hit_parent) || is_node lr) then
              let lrl, n, lrr = splay ~hit_parent x lr in
              (make_node pa ps ll lrl, n, make_node a s lrr r)
            else (ll, l, make_node a s lr r)
      else if a.id < x && ((not hit_parent) || is_node r) then
        match r with
        | T Leaf -> (t, T Leaf, T Leaf)
        | T (Node { loc = pa; state = ps; lt = rl; gt = rr; _ }) ->
            if x < pa.id && ((not hit_parent) || is_node rl) then
              let rll, n, rlr = splay ~hit_parent x rl in
              (make_node a s l rll, n, make_node pa ps rlr rr)
            else if pa.id < x && ((not hit_parent) || is_node rr) then
              let rrl, n, rrr = splay ~hit_parent x rr in
              (make_node pa ps (make_node a s l rl) rrl, n, rrr)
            else (make_node a s l rl, r, rr)
      else (l, t, r)

let[@inline] new_state after =
  { before = Obj.magic (); after; which = W After; awaiters = [] }

let[@inline] eval state =
  match state.which with
  | W Before -> state.before
  | W After -> state.after
  | W (Undetermined _ as which) ->
      if is_after which then state.after else state.before

module Retry = struct
  exception Later

  let[@inline never] later () = raise_notrace Later
  let[@inline] unless condition = if not condition then later ()

  exception Invalid

  let[@inline never] invalid () = raise_notrace Invalid
end

let add_awaiter loc before awaiter =
  (* Fenceless is safe as we have fence after. *)
  let state_old = fenceless_get (as_atomic loc) in
  let state_new =
    let awaiters = awaiter :: state_old.awaiters in
    { before = Obj.magic (); after = before; which = W After; awaiters }
  in
  before == eval state_old
  && Atomic.compare_and_set (as_atomic loc) state_old state_new

let[@tail_mod_cons] rec remove_first x' removed = function
  | [] ->
      removed := false;
      []
  | x :: xs -> if x == x' then xs else x :: remove_first x' removed xs

let rec remove_awaiter backoff loc before awaiter =
  (* Fenceless is safe as we have fence after. *)
  let state_old = fenceless_get (as_atomic loc) in
  if before == eval state_old then
    let removed = ref true in
    let awaiters = remove_first awaiter removed state_old.awaiters in
    if !removed then
      let state_new =
        { before = Obj.magic (); after = before; which = W After; awaiters }
      in
      if not (Atomic.compare_and_set (as_atomic loc) state_old state_new) then
        remove_awaiter (Backoff.once backoff) loc before awaiter

let block timeout loc before =
  let t = Domain_local_await.prepare_for_await () in
  let alive = Timeout.await timeout t.release in
  if add_awaiter loc before t.release then begin
    try t.await ()
    with cancellation_exn ->
      remove_awaiter Backoff.default loc before t.release;
      Timeout.cancel_alive alive;
      raise cancellation_exn
  end;
  Timeout.unawait timeout alive

let rec update_no_alloc timeout backoff loc state f =
  (* Fenceless is safe as we have had a fence before if needed and there is a fence after. *)
  let state_old = fenceless_get (as_atomic loc) in
  let before = eval state_old in
  match f before with
  | after ->
      if before == after then begin
        Timeout.cancel timeout;
        before
      end
      else begin
        state.after <- after;
        if Atomic.compare_and_set (as_atomic loc) state_old state then begin
          resume_awaiters state_old.awaiters;
          Timeout.cancel timeout;
          before
        end
        else update_no_alloc timeout (Backoff.once backoff) loc state f
      end
  | exception Retry.Later ->
      block timeout loc before;
      update_no_alloc timeout backoff loc state f
  | exception exn ->
      Timeout.cancel timeout;
      raise exn

let update_with_state timeout backoff loc f state_old =
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
          resume_awaiters state_old.awaiters;
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

let rec exchange_no_alloc backoff loc state =
  let state_old = atomic_get (as_atomic loc) in
  let before = eval state_old in
  if before == state.after then before
  else if Atomic.compare_and_set (as_atomic loc) state_old state then begin
    resume_awaiters state_old.awaiters;
    before
  end
  else exchange_no_alloc (Backoff.once backoff) loc state

let[@inline] is_obstruction_free which loc =
  (* Fenceless is safe as we are accessing a private location. *)
  fenceless_get (root_as_atomic which) == R Mode.obstruction_free && 0 <= loc.id

let[@inline] rec cas_with_state backoff loc before state state_old =
  before == eval state_old
  && (before == state.after
     ||
     if Atomic.compare_and_set (as_atomic loc) state_old state then begin
       resume_awaiters state_old.awaiters;
       true
     end
     else
       (* We must retry, because compare is by value rather than by state.  In
          other words, we should not fail spuriously due to some other thread
          having installed or removed a waiter.

          Fenceless is safe as there was a fence before. *)
       cas_with_state (Backoff.once backoff) loc before state
         (fenceless_get (as_atomic loc)))

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

  let compare_and_set ?(backoff = Backoff.default) loc before after =
    let state = new_state after in
    let state_old = atomic_get (as_atomic loc) in
    cas_with_state backoff loc before state state_old

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
    let state = atomic_get (as_atomic loc) in
    state.awaiters != []

  let fenceless_get loc = eval (fenceless_get (as_atomic loc))
end

module Xt = struct
  (* NOTE: You can adjust comment blocks below to select whether or not to use
     an unsafe cast to avoid a level of indirection due to [Atomic.t]. *)

  (**)
  type 'x t = {
    mutable _timeout : Timeout.t;
    mutable which : [ `Undetermined ] tdt;
    mutable tree : tree;
    mutable validate_counter : int;
    mutable post_commit : Action.t;
  }

  let[@inline] timeout_unset () = Timeout.Unset

  external timeout_as_atomic : 'x t -> Timeout.t Atomic.t = "%identity"
  (**)

  (*
  type 'x t = {
    mutable _timeout : Timeout.t Atomic.t;
    mutable which : [ `Undetermined ] tdt;
    mutable tree : tree;
    mutable validate_counter : int;
    mutable post_commit : Action.t;
  }

  let[@inline] timeout_unset () = Atomic.make Timeout.Unset
  let[@inline] timeout_as_atomic r = r._timeout
  *)

  let[@inline] validate_one which loc state =
    let before = if is_cmp which state then eval state else state.before in
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    if before != eval (fenceless_get (as_atomic loc)) then Retry.invalid ()

  let rec validate_all which = function
    | T Leaf -> ()
    | T (Node node_r) ->
        if is_node node_r.lt then validate_all which node_r.lt;
        validate_one which node_r.loc node_r.state;
        validate_all which node_r.gt

  let[@inline] maybe_validate_log xt =
    let c0 = xt.validate_counter in
    let c1 = c0 + 1 in
    xt.validate_counter <- c1;
    (* Validate whenever counter reaches next power of 2. *)
    if c0 land c1 = 0 then begin
      Timeout.check (timeout_as_atomic xt);
      validate_all xt.which xt.tree
    end

  let[@inline] update_new loc f xt lt gt =
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    let state = fenceless_get (as_atomic loc) in
    let before = eval state in
    match f before with
    | after ->
        let state =
          if before == after && is_obstruction_free xt.which loc then state
          else { before; after; which = W xt.which; awaiters = [] }
        in
        xt.tree <- T (Node { loc; state; lt; gt; awaiters = [] });
        before
    | exception exn ->
        xt.tree <- T (Node { loc; state; lt; gt; awaiters = [] });
        raise exn

  let[@inline] update_top loc f xt state' lt gt =
    let state = Obj.magic state' in
    if is_cmp xt.which state then begin
      let before = eval state in
      let after = f before in
      let state =
        if before == after then state
        else { before; after; which = W xt.which; awaiters = [] }
      in
      xt.tree <- T (Node { loc; state; lt; gt; awaiters = [] });
      before
    end
    else
      let current = state.after in
      let state = { state with after = f current } in
      xt.tree <- T (Node { loc; state; lt; gt; awaiters = [] });
      current

  let[@inline] unsafe_update ~xt loc f =
    maybe_validate_log xt;
    let x = loc.id in
    match xt.tree with
    | T Leaf -> update_new loc f xt (T Leaf) (T Leaf)
    | T (Node { loc = a; lt = T Leaf; _ }) as tree when x < a.id ->
        update_new loc f xt (T Leaf) tree
    | T (Node { loc = a; gt = T Leaf; _ }) as tree when a.id < x ->
        update_new loc f xt tree (T Leaf)
    | T (Node { loc = a; state; lt; gt; _ }) when Obj.magic a == loc ->
        update_top loc f xt state lt gt
    | tree -> begin
        match splay ~hit_parent:false x tree with
        | l, T Leaf, r -> update_new loc f xt l r
        | l, T (Node node_r), r -> update_top loc f xt node_r.state l r
      end

  let[@inline] protect xt f x =
    let tree = xt.tree in
    let y = f x in
    assert (xt.tree == tree);
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
    match xt.tree with
    | T Leaf -> ()
    | T (Node { loc = a; lt = T Leaf; _ }) when x < a.id -> ()
    | T (Node { loc = a; gt = T Leaf; _ }) when a.id < x -> ()
    | T (Node { loc = a; state; _ }) when Obj.magic a == loc ->
        validate_one xt.which a state
    | tree -> begin
        match splay ~hit_parent:true x tree with
        | lt, T (Node node_r), gt ->
            xt.tree <- T (Node { node_r with lt; gt; awaiters = [] });
            if Obj.magic node_r.loc == loc then
              validate_one xt.which node_r.loc node_r.state
        | _, T Leaf, _ -> impossible ()
      end

  let is_in_log ~xt loc =
    let x = loc.id in
    match xt.tree with
    | T Leaf -> false
    | T (Node { loc = a; lt = T Leaf; _ }) when x < a.id -> false
    | T (Node { loc = a; gt = T Leaf; _ }) when a.id < x -> false
    | T (Node { loc = a; _ }) when Obj.magic a == loc -> true
    | tree -> begin
        match splay ~hit_parent:true x tree with
        | lt, T (Node node_r), gt ->
            xt.tree <- T (Node { node_r with lt; gt; awaiters = [] });
            Obj.magic node_r.loc == loc
        | _, T Leaf, _ -> impossible ()
      end

  let rec rollback which tree_snap tree =
    if tree_snap == tree then tree
    else
      match tree with
      | T Leaf -> T Leaf
      | T (Node node_r) -> begin
          match splay ~hit_parent:false node_r.loc.id tree_snap with
          | lt_mark, T Leaf, gt_mark ->
              let lt = rollback which lt_mark node_r.lt
              and gt = rollback which gt_mark node_r.gt in
              let state =
                let state = node_r.state in
                if is_cmp which state then state
                else
                  (* Fenceless is safe inside transactions as each log update
                     has a fence. *)
                  let current = fenceless_get (as_atomic node_r.loc) in
                  if state.before != eval current then Retry.invalid ()
                  else current
              in
              T (Node { loc = node_r.loc; state; lt; gt; awaiters = [] })
          | lt_mark, T (Node inner_node_r), gt_mark ->
              let lt = rollback which lt_mark node_r.lt
              and gt = rollback which gt_mark node_r.gt in
              T (Node { inner_node_r with lt; gt; awaiters = [] })
        end

  type 'x snap = tree * Action.t

  let snapshot ~xt = (xt.tree, xt.post_commit)

  let rollback ~xt (snap, post_commit) =
    xt.tree <- rollback xt.which snap xt.tree;
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

  let rec add_awaiters awaiter which = function
    | T Leaf -> T Leaf
    | T (Node node_r) as stop -> begin
        match
          if is_node node_r.lt then add_awaiters awaiter which node_r.lt
          else node_r.lt
        with
        | T Leaf ->
            if
              add_awaiter node_r.loc
                (let state = node_r.state in
                 if is_cmp which state then eval state else state.before)
                awaiter
            then add_awaiters awaiter which node_r.gt
            else stop
        | T (Node _) as stop -> stop
      end

  let rec remove_awaiters awaiter which stop = function
    | T Leaf -> ()
    | T (Node node_r) as current ->
        if is_node node_r.lt then remove_awaiters awaiter which stop node_r.lt;
        if current != stop then begin
          remove_awaiter Backoff.default node_r.loc
            (let state = node_r.state in
             if is_cmp which state then eval state else state.before)
            awaiter;
          remove_awaiters awaiter which stop node_r.gt
        end

  let initial_validate_period = 16

  let[@inline] reset_quick xt =
    xt.tree <- T Leaf;
    xt.validate_counter <- initial_validate_period;
    xt.post_commit <- Action.noop;
    xt

  let reset mode xt =
    xt.which <- Undetermined { root = R mode };
    reset_quick xt

  let success xt result =
    Timeout.cancel (timeout_as_atomic xt);
    Action.run xt.post_commit result

  let rec commit backoff mode xt tx =
    match tx ~xt with
    | result -> begin
        match xt.tree with
        | T Leaf -> success xt result
        | T (Node { loc; state; lt = T Leaf; gt = T Leaf; _ }) ->
            if is_cmp xt.which state then success xt result
            else begin
              state.which <- W After;
              let before = state.before in
              if isnt_int before then state.before <- Obj.magic ();
              (* Fenceless is safe inside transactions as each log update has a
                 fence. *)
              let state_old = fenceless_get (as_atomic loc) in
              if cas_with_state Backoff.default loc before state state_old then
                success xt result
              else commit (Backoff.once backoff) mode (reset_quick xt) tx
            end
        | T (Node node_r) -> begin
            let root = Node node_r in
            (* Fenceless is safe as [which] is private at this point. *)
            fenceless_set (root_as_atomic xt.which) (R root);
            (* The end result is a cyclic data structure, which is why we cannot
               initialize the [which] atomic directly. *)
            match determine xt.which 0 (T root) with
            | status ->
                if a_cmp_followed_by_a_cas < status then begin
                  if finish xt.which root (verify xt.which (T root)) then
                    success xt result
                  else begin
                    (* We switch to [Mode.lock_free] as there was
                       interference. *)
                    commit (Backoff.once backoff) Mode.lock_free
                      (reset Mode.lock_free xt) tx
                  end
                end
                else if
                  a_cmp = status
                  || finish xt.which root
                       (if 0 <= status then After else Before)
                then success xt result
                else commit (Backoff.once backoff) mode (reset mode xt) tx
            | exception Exit ->
                (* Fenceless is safe as there was a fence before. *)
                if fenceless_get (root_as_atomic xt.which) == R After then
                  success xt result
                else commit (Backoff.once backoff) mode (reset mode xt) tx
          end
      end
    | exception Retry.Invalid ->
        Timeout.check (timeout_as_atomic xt);
        commit (Backoff.once backoff) mode (reset_quick xt) tx
    | exception Retry.Later -> begin
        if xt.tree == T Leaf then invalid_retry ();
        let t = Domain_local_await.prepare_for_await () in
        let alive = Timeout.await (timeout_as_atomic xt) t.release in
        match add_awaiters t.release xt.which xt.tree with
        | T Leaf -> begin
            match t.await () with
            | () ->
                remove_awaiters t.release xt.which (T Leaf) xt.tree;
                Timeout.unawait (timeout_as_atomic xt) alive;
                commit (Backoff.reset backoff) mode (reset_quick xt) tx
            | exception cancellation_exn ->
                remove_awaiters t.release xt.which (T Leaf) xt.tree;
                Timeout.cancel_alive alive;
                raise cancellation_exn
          end
        | T (Node _) as stop ->
            remove_awaiters t.release xt.which stop xt.tree;
            Timeout.unawait (timeout_as_atomic xt) alive;
            commit (Backoff.once backoff) mode (reset_quick xt) tx
      end
    | exception exn ->
        Timeout.cancel (timeout_as_atomic xt);
        raise exn

  let[@inline] commit ?timeoutf ?(backoff = Backoff.default)
      ?(mode = Mode.obstruction_free) tx =
    let _timeout = timeout_unset ()
    and which = Undetermined { root = R mode }
    and tree = T Leaf
    and validate_counter = initial_validate_period
    and post_commit = Action.noop in
    let xt = { _timeout; which; tree; validate_counter; post_commit } in
    Timeout.set_opt (timeout_as_atomic xt) timeoutf;
    commit backoff mode xt tx.tx
end
