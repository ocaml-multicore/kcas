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

let[@inline] fenceless_get x =
  fenceless_get ((* Prevents CSE *) Sys.opaque_identity x)
(**)
(*
let fenceless_get = atomic_get
*)

module Timeout = struct
  exception Timeout

  let[@inline never] timeout () = raise Timeout

  type _ t =
    | Unset : [> `Unset ] t
    | Elapsed : [> `Elapsed ] t
    | Call : (unit -> unit) -> [> `Call ] t
    | Set : { mutable state : [< `Elapsed | `Call ] t } -> [> `Set ] t

  external as_atomic : [< `Set ] t -> [< `Elapsed | `Call ] t Atomic.t
    = "%identity"

  (* Fenceless operations are safe here as the timeout state is not not visible
     outside of the library and we don't always need the latest value and, when
     we do, there is a fence after. *)

  let[@inline] check (t : [< `Set | `Unset ] t) =
    match t with
    | Unset -> ()
    | Set set_r ->
        if fenceless_get (as_atomic (Set set_r)) == Elapsed then timeout ()

  let set seconds (state : [< `Elapsed | `Call ] t Atomic.t) =
    Domain_local_timeout.set_timeoutf seconds @@ fun () ->
    match Atomic.exchange state Elapsed with
    | Call release_or_cancel -> release_or_cancel ()
    | Elapsed -> ()

  let call_id = Call Fun.id

  let[@inline never] alloc seconds =
    let (Set set_r as t : [ `Set ] t) = Set { state = call_id } in
    let cancel = set seconds (as_atomic t) in
    if not (Atomic.compare_and_set (as_atomic t) call_id (Call cancel)) then
      timeout ();
    Set set_r

  let[@inline] alloc_opt = function
    | None -> Unset
    | Some seconds -> alloc seconds

  let[@inline never] await (state : [< `Elapsed | `Call ] t Atomic.t) release =
    match fenceless_get state with
    | Call cancel as alive ->
        if Atomic.compare_and_set state alive (Call release) then Call cancel
        else timeout ()
    | Elapsed -> timeout ()

  let[@inline] await (t : [ `Unset | `Set ] t) release =
    match t with Unset -> Unset | Set r -> await (as_atomic (Set r)) release

  let[@inline never] unawait (state : [< `Elapsed | `Call ] t Atomic.t) alive =
    match fenceless_get state with
    | Call _ as await ->
        if not (Atomic.compare_and_set state await alive) then timeout ()
    | Elapsed -> timeout ()

  let[@inline] unawait t alive =
    match (t, alive) with
    | Set set_r, Call call_r -> unawait (as_atomic (Set set_r)) (Call call_r)
    | _ -> ()

  let[@inline] cancel_alive (alive : [< `Unset | `Call ] t) =
    match alive with Unset -> () | Call cancel -> cancel ()

  let[@inline] cancel (t : [< `Set | `Unset ] t) =
    match t with
    | Unset -> ()
    | Set set_r -> (
        match fenceless_get (as_atomic (Set set_r)) with
        | Elapsed -> ()
        | Call cancel -> cancel ())
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

module Mode = struct
  type t = [ `Lock_free | `Obstruction_free ]
end

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
  | Xt : {
      mutable rot : rot;
          (** [rot] is for Root or Tree.

              This field must be first, see [root_as_atomic] and
              [tree_as_ref]. *)
      timeout : [ `Set | `Unset ] Timeout.t;
      mutable mode : Mode.t;
      mutable validate_counter : int;
      mutable post_commit : Action.t;
    }
      -> [> `Xt ] tdt
      (** The result might not yet have been determined.  The [root] either says
          which it is or points to the root of the transaction log or [tree].

          Note that if/when local/stack allocation mode becomes available in
          OCaml, the transaction log should be mostly stack allocated. *)
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
and rot = U : [< `Before | `After | `Node | `Leaf ] tdt -> rot [@@unboxed]
and which = W : [< `Before | `After | `Xt ] tdt -> which [@@unboxed]

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

external root_as_atomic : [< `Xt ] tdt -> root Atomic.t = "%identity"
external tree_as_ref : [< `Xt ] tdt -> tree ref = "%identity"

let[@inline] is_node tree = tree != T Leaf
let[@inline] is_cmp which state = state.which != W which
let[@inline] is_cas which state = state.which == W which

let[@inline] is_determined = function
  | (Xt _ as xt : [< `Xt ] tdt) -> begin
      match fenceless_get (root_as_atomic xt) with
      | R (Node _) -> false
      | R After | R Before -> true
    end

let[@inline] isnt_int x = not (Obj.is_int (Obj.repr x))

let[@inline] rec release_after_rec which = function
  | T Leaf -> true
  | T (Node node_r) -> release_after which (Node node_r)

and release_after which (Node node_r : [< `Node ] tdt) =
  release_after_rec which node_r.lt |> ignore;
  let state = node_r.state in
  if is_cas which state then begin
    state.which <- W After;
    if isnt_int state.before then state.before <- Obj.magic ();
    resume_awaiters node_r.awaiters
  end;
  release_after_rec which node_r.gt

let[@inline] rec release_before_rec which = function
  | T Leaf -> false
  | T (Node node_r) -> release_before which (Node node_r)

and release_before which (Node node_r : [< `Node ] tdt) =
  release_before_rec which node_r.lt |> ignore;
  let state = node_r.state in
  if is_cas which state then begin
    state.which <- W Before;
    if isnt_int state.after then state.after <- Obj.magic ();
    resume_awaiters node_r.awaiters
  end;
  release_before_rec which node_r.gt

let release which tree status =
  if status == After then release_after which tree
  else release_before which tree

let[@inline] rec verify_rec which = function
  | T Leaf -> After
  | T (Node node_r) -> verify which (Node node_r)

and verify which (Node node_r : [< `Node ] tdt) =
  let status = verify_rec which node_r.lt in
  if status == After then
    (* Fenceless is safe as [finish] has a fence after. *)
    if
      is_cmp which node_r.state
      && fenceless_get (as_atomic node_r.loc) != node_r.state
    then Before
    else verify_rec which node_r.gt
  else status

let finish which root status =
  if Atomic.compare_and_set (root_as_atomic which) (R root) (R status) then
    release which root status
  else
    (* Fenceless is safe as we have a fence above. *)
    fenceless_get (root_as_atomic which) == R After

let a_cmp = 1
let a_cas = 2
let a_cmp_followed_by_a_cas = 4

let[@inline] next_status a_cas_or_a_cmp status =
  let a_cmp_followed_by_a_cas = a_cas_or_a_cmp * 2 land (status * 4) in
  status lor a_cas_or_a_cmp lor a_cmp_followed_by_a_cas

let[@inline] rec determine_rec which status = function
  | T Leaf -> status
  | T (Node node_r) -> determine which status (Node node_r)

and determine which status (Node node_r : [< `Node ] tdt) =
  let status = determine_rec which status node_r.lt in
  if status < 0 then status
  else determine_eq Backoff.default which status (Node node_r)

and determine_eq backoff which status (Node node_r as eq : [< `Node ] tdt) =
  let current = atomic_get (as_atomic node_r.loc) in
  let state = node_r.state in
  if state == current then begin
    let a_cas_or_a_cmp = 1 + Bool.to_int (is_cas which state) in
    if is_determined which then raise_notrace Exit;
    determine_rec which (next_status a_cas_or_a_cmp status) node_r.gt
  end
  else
    let matches_expected () =
      match current.which with
      | W Before -> state.before == current.before
      | W After -> state.before == current.after
      | W (Xt _ as xt) ->
          if is_after xt then state.before == current.after
          else state.before == current.before
    in
    if is_cas which state && matches_expected () then begin
      if is_determined which then raise_notrace Exit;
      (* We now know that the operation wasn't finished when we read [current],
         but it is possible that the [loc]ation has been updated since then by
         some other domain helping us (or even by some later operation).  If so,
         then the [compare_and_set] below fails.  Copying the awaiters from
         [current] is safe in either case, because we know that we have the
         [current] state that our operation is interested in.  By doing the
         copying here, we at most duplicate work already done by some other
         domain.  However, it is necessary to do the copy before the
         [compare_and_set], because afterwards is too late as some other domain
         might finish the operation after the [compare_and_set] and miss the
         awaiters. *)
      if current.awaiters != [] then node_r.awaiters <- current.awaiters;
      if Atomic.compare_and_set (as_atomic node_r.loc) current state then
        determine_rec which (next_status a_cas status) node_r.gt
      else determine_eq (Backoff.once backoff) which status eq
    end
    else -1

and is_after = function
  | (Xt _ as xt : [< `Xt ] tdt) -> begin
      (* Fenceless at most gives old root and causes extra work. *)
      match fenceless_get (root_as_atomic xt) with
      | R (Node node_r) -> begin
          let root = Node node_r in
          match determine xt 0 root with
          | status ->
              finish xt root
                (if a_cmp_followed_by_a_cas < status then verify xt root
                 else if 0 <= status then After
                 else Before)
          | exception Exit ->
              (* Fenceless is safe as there was a fence before. *)
              fenceless_get (root_as_atomic xt) == R After
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
  | W (Xt _ as xt) -> if is_after xt then state.after else state.before

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

  let make ?(padded = false) ?(mode = `Obstruction_free) after =
    let state = new_state after
    and id = if mode == `Obstruction_free then Id.nat_id () else Id.neg_id () in
    make_loc padded state id

  let make_contended ?mode after = make ~padded:true ?mode after

  let make_array ?(padded = false) ?(mode = `Obstruction_free) n after =
    assert (0 <= n);
    let state = new_state after
    and id =
      (if mode == `Obstruction_free then Id.nat_ids n else Id.neg_ids n)
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
    if loc.id < 0 then `Lock_free else `Obstruction_free

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
  type 'x t = [ `Xt ] tdt

  let[@inline] validate_one which loc state =
    let before = if is_cmp which state then eval state else state.before in
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    if before != eval (fenceless_get (as_atomic loc)) then Retry.invalid ()

  let[@inline] rec validate_all_rec which = function
    | T Leaf -> ()
    | T (Node node_r) -> validate_all which (Node node_r)

  and validate_all which (Node node_r : [< `Node ] tdt) =
    validate_all_rec which node_r.lt;
    validate_one which node_r.loc node_r.state;
    validate_all_rec which node_r.gt

  let[@inline] maybe_validate_log (Xt xt_r as xt : _ t) =
    let c0 = xt_r.validate_counter in
    let c1 = c0 + 1 in
    xt_r.validate_counter <- c1;
    (* Validate whenever counter reaches next power of 2. *)
    if c0 land c1 = 0 then begin
      Timeout.check xt_r.timeout;
      validate_all_rec xt !(tree_as_ref xt)
    end

  let[@inline] is_obstruction_free (Xt xt_r : _ t) loc =
    (* Fenceless is safe as we are accessing a private location. *)
    xt_r.mode == `Obstruction_free && 0 <= loc.id

  let[@inline] update_new loc f xt lt gt =
    (* Fenceless is safe inside transactions as each log update has a fence. *)
    let state = fenceless_get (as_atomic loc) in
    let before = eval state in
    match f before with
    | after ->
        let state =
          if before == after && is_obstruction_free xt loc then state
          else { before; after; which = W xt; awaiters = [] }
        in
        tree_as_ref xt := T (Node { loc; state; lt; gt; awaiters = [] });
        before
    | exception exn ->
        tree_as_ref xt := T (Node { loc; state; lt; gt; awaiters = [] });
        raise exn

  let[@inline] update_top loc f xt state' lt gt =
    let state = Obj.magic state' in
    if is_cmp xt state then begin
      let before = eval state in
      let after = f before in
      let state =
        if before == after then state
        else { before; after; which = W xt; awaiters = [] }
      in
      tree_as_ref xt := T (Node { loc; state; lt; gt; awaiters = [] });
      before
    end
    else
      let current = state.after in
      let state = { state with after = f current } in
      tree_as_ref xt := T (Node { loc; state; lt; gt; awaiters = [] });
      current

  let[@inline] unsafe_update ~xt loc f =
    maybe_validate_log xt;
    let x = loc.id in
    match !(tree_as_ref xt) with
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
    let tree = !(tree_as_ref xt) in
    let y = f x in
    assert (!(tree_as_ref xt) == tree);
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

  let post_commit ~xt:(Xt xt_r : _ t) action =
    xt_r.post_commit <- Action.append action xt_r.post_commit

  let validate ~xt loc =
    let x = loc.id in
    match !(tree_as_ref xt) with
    | T Leaf -> ()
    | T (Node { loc = a; lt = T Leaf; _ }) when x < a.id -> ()
    | T (Node { loc = a; gt = T Leaf; _ }) when a.id < x -> ()
    | T (Node { loc = a; state; _ }) when Obj.magic a == loc ->
        validate_one xt a state
    | tree -> begin
        match splay ~hit_parent:true x tree with
        | lt, T (Node node_r), gt ->
            tree_as_ref xt := T (Node { node_r with lt; gt; awaiters = [] });
            if Obj.magic node_r.loc == loc then
              validate_one xt node_r.loc node_r.state
        | _, T Leaf, _ -> impossible ()
      end

  let is_in_log ~xt loc =
    let x = loc.id in
    match !(tree_as_ref xt) with
    | T Leaf -> false
    | T (Node { loc = a; lt = T Leaf; _ }) when x < a.id -> false
    | T (Node { loc = a; gt = T Leaf; _ }) when a.id < x -> false
    | T (Node { loc = a; _ }) when Obj.magic a == loc -> true
    | tree -> begin
        match splay ~hit_parent:true x tree with
        | lt, T (Node node_r), gt ->
            tree_as_ref xt := T (Node { node_r with lt; gt; awaiters = [] });
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

  let snapshot ~xt:(Xt xt_r as xt : _ t) = (!(tree_as_ref xt), xt_r.post_commit)

  let rollback ~xt:(Xt xt_r as xt : _ t) (snap, post_commit) =
    tree_as_ref xt := rollback xt snap !(tree_as_ref xt);
    xt_r.post_commit <- post_commit

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

  let[@inline] rec add_awaiters_rec awaiter which = function
    | T Leaf -> T Leaf
    | T (Node node_r) -> add_awaiters awaiter which (Node node_r)

  and add_awaiters awaiter which (Node node_r as stop : [< `Node ] tdt) =
    match add_awaiters_rec awaiter which node_r.lt with
    | T Leaf ->
        if
          add_awaiter node_r.loc
            (let state = node_r.state in
             if is_cmp which state then eval state else state.before)
            awaiter
        then add_awaiters_rec awaiter which node_r.gt
        else T stop
    | T (Node _) as stop -> stop

  let[@inline] rec remove_awaiters_rec awaiter which stop = function
    | T Leaf -> T Leaf
    | T (Node node_r) -> remove_awaiters awaiter which stop (Node node_r)

  and remove_awaiters awaiter which stop (Node node_r as at : [< `Node ] tdt) =
    match remove_awaiters_rec awaiter which stop node_r.lt with
    | T Leaf ->
        if T at != stop then begin
          remove_awaiter Backoff.default node_r.loc
            (let state = node_r.state in
             if is_cmp which state then eval state else state.before)
            awaiter;
          remove_awaiters_rec awaiter which stop node_r.gt
        end
        else stop
    | T (Node _) as stop -> stop

  let initial_validate_period = 16

  let success (Xt xt_r : _ t) result =
    Timeout.cancel xt_r.timeout;
    Action.run xt_r.post_commit result

  let rec commit backoff (Xt xt_r as xt : _ t) tx =
    match tx ~xt with
    | result -> begin
        match !(tree_as_ref xt) with
        | T Leaf -> success xt result
        | T (Node { loc; state; lt = T Leaf; gt = T Leaf; _ }) ->
            if is_cmp xt state then success xt result
            else begin
              state.which <- W After;
              let before = state.before in
              if isnt_int before then state.before <- Obj.magic ();
              (* Fenceless is safe inside transactions as each log update has a
                 fence. *)
              let state_old = fenceless_get (as_atomic loc) in
              if cas_with_state Backoff.default loc before state state_old then
                success xt result
              else commit_once_reuse backoff xt tx
            end
        | T (Node node_r) -> begin
            let root = Node node_r in
            match determine xt 0 root with
            | status ->
                if a_cmp_followed_by_a_cas < status then begin
                  if finish xt root (verify xt root) then success xt result
                  else begin
                    (* We switch to [`Lock_free] as there was
                       interference. *)
                    commit_once_alloc backoff `Lock_free xt tx
                  end
                end
                else if
                  a_cmp = status
                  || finish xt root (if 0 <= status then After else Before)
                then success xt result
                else commit_once_alloc backoff xt_r.mode xt tx
            | exception Exit ->
                (* Fenceless is safe as there was a fence before. *)
                if fenceless_get (root_as_atomic xt) == R After then
                  success xt result
                else commit_once_alloc backoff xt_r.mode xt tx
          end
      end
    | exception Retry.Invalid -> commit_once_reuse backoff xt tx
    | exception Retry.Later -> begin
        match !(tree_as_ref xt) with
        | T Leaf -> invalid_retry ()
        | T (Node node_r) -> begin
            let root = Node node_r in
            let t = Domain_local_await.prepare_for_await () in
            let alive = Timeout.await xt_r.timeout t.release in
            match add_awaiters t.release xt root with
            | T Leaf -> begin
                match t.await () with
                | () ->
                    remove_awaiters t.release xt (T Leaf) root |> ignore;
                    Timeout.unawait xt_r.timeout alive;
                    commit_reset_reuse backoff xt tx
                | exception cancellation_exn ->
                    remove_awaiters t.release xt (T Leaf) root |> ignore;
                    Timeout.cancel_alive alive;
                    raise cancellation_exn
              end
            | T (Node _) as stop ->
                remove_awaiters t.release xt stop root |> ignore;
                Timeout.unawait xt_r.timeout alive;
                commit_once_reuse backoff xt tx
          end
      end
    | exception exn ->
        Timeout.cancel xt_r.timeout;
        raise exn

  and commit_once_reuse backoff xt tx =
    commit_reuse (Backoff.once backoff) xt tx

  and commit_reset_reuse backoff xt tx =
    commit_reuse (Backoff.reset backoff) xt tx

  and commit_reuse backoff (Xt xt_r as xt : _ t) tx =
    tree_as_ref xt := T Leaf;
    xt_r.validate_counter <- initial_validate_period;
    xt_r.post_commit <- Action.noop;
    Timeout.check xt_r.timeout;
    commit backoff xt tx

  and commit_once_alloc backoff mode (Xt xt_r : _ t) tx =
    let backoff = Backoff.once backoff in
    Timeout.check xt_r.timeout;
    let rot = U Leaf in
    let validate_counter = initial_validate_period in
    let post_commit = Action.noop in
    let xt = Xt { xt_r with rot; mode; validate_counter; post_commit } in
    commit backoff xt tx

  let[@inline] commit ?timeoutf ?(backoff = Backoff.default)
      ?(mode = `Obstruction_free) { tx } =
    let timeout = Timeout.alloc_opt timeoutf
    and rot = U Leaf
    and validate_counter = initial_validate_period
    and post_commit = Action.noop in
    let xt = Xt { rot; timeout; mode; validate_counter; post_commit } in
    commit backoff xt tx
end
