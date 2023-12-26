(** This library provides a software transactional memory (STM) implementation
    based on an atomic lock-free multi-word compare-and-set (MCAS) algorithm
    enhanced with read-only compare operations and ability to block awaiting for
    changes.

    Features and properties:

    - {b Efficient}: In the common uncontended case only [k + 1] single-word
      CASes are required per [k]-CAS and, as a special case, [1]-CAS requires
      only a single single-word CAS.

    - {b Lock-free}: The underlying algorithm guarantees that at least one
      operation will be able to make progress.

    - {b Disjoint-access parallel}: Unrelated operations progress independently,
      without interference, even if they occur at the same time.

    - {b Read-only compares}: The algorithm supports
      {{:https://en.wikipedia.org/wiki/Non-blocking_algorithm#Obstruction-freedom}
      obstruction-free} read-only compare (CMP) operations that can be performed
      on overlapping locations in parallel without interference.

    - {b Blocking await}: The algorithm supports timeouts and awaiting for
      changes to any number of shared memory locations.

    - {b Composable}: Independently developed transactions can be composed with
      ease sequentially, conjunctively, conditionally, and disjunctively.

    In other words, performance should be acceptable and scalable for many use
    cases, the non-blocking properties should allow use in many contexts
    including those where locks are not acceptable, and the features provided
    should support most practical needs.

    {1 A quick tour}

    Let's first open the library for convenience:

    {[
      open Kcas
    ]}

    To use the library one creates shared memory locations:

    {[
      # let a = Loc.make 0
        and b = Loc.make 0
        and x = Loc.make 0
      val a : int Loc.t = <abstr>
      val b : int Loc.t = <abstr>
      val x : int Loc.t = <abstr>
    ]}

    One can then manipulate the locations individually:

    {[
      # Loc.set a 10
      - : unit = ()

      # Loc.get a
      - : int = 10

      # Loc.compare_and_set b 0 52
      - : bool = true

      # Loc.get b
      - : int = 52
    ]}

    Block waiting for changes to locations:

    {[
      # let a_domain = Domain.spawn @@ fun () ->
          let x = Loc.get_as (fun x -> Retry.unless (x <> 0); x) x in
          Printf.sprintf "The answer is %d!" x
      val a_domain : string Domain.t = <abstr>
    ]}

    Perform transactions over locations:

    {[
      # let tx ~xt =
          let a = Xt.get ~xt a
          and b = Xt.get ~xt b in
          Xt.set ~xt x (b - a)
        in
        Xt.commit { tx }
      - : unit = ()
    ]}

    And now we have it:

    {[
      # Domain.join a_domain
      - : string = "The answer is 42!"
    ]}

    The main repository includes a longer introduction with many examples and
    discussion of more advanced topics for designing lock-free algorithms. *)

(** {1 Auxiliary modules}

    The modules in this section serve auxiliary purposes.  On a first read you
    can skip over these.  The documentation links back to these modules where
    appropriate. *)

(** Timeout support. *)
module Timeout : sig
  exception Timeout
  (** Exception that may be raised by operations such as {!Loc.get_as},
      {!Loc.update}, {!Loc.modify}, or {!Xt.commit} when given a [~timeoutf] in
      seconds. *)
end

(** Retry support. *)
module Retry : sig
  exception Later
  (** Exception that may be raised to signal that the operation, such as
      {!Loc.get_as}, {!Loc.update}, or {!Xt.commit}, should be retried, at some
      point in the future, after the examined shared memory location or
      locations have changed.

      {b NOTE}: It is important to understand that "{i after}" may effectively
      mean "{i immediately}", because it may be the case that the examined
      shared memory locations have already changed. *)

  val later : unit -> 'a
  (** [later ()] is equivalent to [raise Later]. *)

  val unless : bool -> unit
  (** [unless condition] is equivalent to [if not condition then later ()]. *)

  exception Invalid
  (** Exception that may be raised to signal that the transaction log is no
      longer valid, e.g. because shared memory locations have been changed
      outside of the transaction, and the transaction should be retried. *)

  val invalid : unit -> 'a
  (** [invalid ()] is equivalent to [raise Invalid]. *)
end

(** Operating modes of the [k-CAS-n-CMP] algorithm. *)
module Mode : sig
  type t =
    [ `Lock_free
      (** In [`Lock_free] mode the algorithm makes sure that at least one domain will
      be able to make progress at the cost of performing read-only operations as
      read-write operations. *)
    | `Obstruction_free
      (** In [`Obstruction_free] mode the algorithm proceeds optimistically and
      allows read-only operations to fail due to interference from other domains
      that might have been prevented in the [`Lock_free] mode. *)
    ]
  (** Type of an operating mode of the [k-CAS-n-CMP] algorithm. *)
end

(** {1 Individual locations}

    Individual shared memory locations can be created and manipulated through
    the {!Loc} module that is essentially compatible with the [Stdlib.Atomic]
    module except that some of the operations take additional optional
    arguments:

    - [backoff] specifies the configuration for the [Backoff] mechanism.  In
      special cases, having more detailed knowledge of the application, one
      might adjust the configuration to improve performance.

    - [timeoutf] specifies a timeout in seconds and, if specified, the
      {!Timeout.Timeout} exception may be raised by the operation to signal that
      the timeout expired. *)

(** Shared memory locations. *)
module Loc : sig
  type !'a t
  (** Type of shared memory locations. *)

  val make : ?padded:bool -> ?mode:Mode.t -> 'a -> 'a t
  (** [make initial] creates a new shared memory location with the [initial]
      value.

      The optional [padded] argument defaults to [false].  If explicitly
      specified as [~padded:true] the location will be allocated in a way to
      avoid false sharing.  For relatively long lived shared memory locations
      this can improve performance and make performance more stable at the cost
      of using more memory.  It is not recommended to use [~padded:true] for
      short lived shared memory locations.

      The optional {{!Mode.t} [mode]} argument defaults to [`Obstruction_free].
      If explicitly specified as [`Lock_free], the location will always be
      accessed using the lock-free operating mode.  This may improve performance
      in rare cases where a location is updated frequently and obstruction-free
      read-only accesses would almost certainly suffer from interference. *)

  val make_contended : ?mode:Mode.t -> 'a -> 'a t
  (** [make_contended initial] is equivalent to [make ~padded:true initial]. *)

  val make_array : ?padded:bool -> ?mode:Mode.t -> int -> 'a -> 'a t array
  (** [make_array n initial] creates an array of [n] new shared memory locations
      with the [initial] value. *)

  val get_mode : 'a t -> Mode.t
  (** [get_mode r] returns the operating mode of the shared memory location
      [r]. *)

  val get_id : 'a t -> int
  (** [get_id r] returns the unique id of the shared memory location [r]. *)

  val get : 'a t -> 'a
  (** [get r] reads the current value of the shared memory location [r]. *)

  val get_as : ?timeoutf:float -> ('a -> 'b) -> 'a t -> 'b
  (** [get_as f loc] is equivalent to [f (get loc)].  The given function [f] may
      raise the {!Retry.Later} exception to signal that the conditional load
      should be retried only after the location has been modified outside of the
      conditional load.  It is also safe for the given function [f] to raise any
      other exception to abort the conditional load. *)

  val compare_and_set : ?backoff:Backoff.t -> 'a t -> 'a -> 'a -> bool
  (** [compare_and_set r before after] atomically updates the shared memory
      location [r] to the [after] value if the current value of [r] is the
      [before] value. *)

  val update : ?timeoutf:float -> ?backoff:Backoff.t -> 'a t -> ('a -> 'a) -> 'a
  (** [update r f] repeats [let b = get r in compare_and_set r b (f b)] until it
      succeeds and then returns the [b] value.  The given function [f] may raise
      the {!Retry.Later} exception to signal that the update should only be
      retried after the location has been modified outside of the update.  It is
      also safe for the given function [f] to raise any other exception to abort
      the update. *)

  val modify :
    ?timeoutf:float -> ?backoff:Backoff.t -> 'a t -> ('a -> 'a) -> unit
  (** [modify r f] is equivalent to [update r f |> ignore]. *)

  val exchange : ?backoff:Backoff.t -> 'a t -> 'a -> 'a
  (** [exchange r after] atomically updates the shared memory location [r] to
      the [after] value and returns the current value (before the exchange). *)

  val set : ?backoff:Backoff.t -> 'a t -> 'a -> unit
  (** [set r after] atomically updates the shared memory location [r] to the
      [after] value. *)

  val fetch_and_add : ?backoff:Backoff.t -> int t -> int -> int
  (** [fetch_and_add r n] atomically increments the value of [r] by [n], and
      returns the current value (before the increment). *)

  val incr : ?backoff:Backoff.t -> int t -> unit
  (** [incr r] atomically increments [r]. *)

  val decr : ?backoff:Backoff.t -> int t -> unit
  (** [decr r] atomically decrements [r]. *)

  (**/**)

  val has_awaiters : 'a t -> bool
  (** [has_awaiters r] determines whether the shared memory location [r] has
      awaiters. *)

  val fenceless_get : 'a t -> 'a
  (** [fenceless_get r] is like [get r] except that [fenceless_get]s may be
      reordered. *)

  val fenceless_update :
    ?timeoutf:float -> ?backoff:Backoff.t -> 'a t -> ('a -> 'a) -> 'a
  (** [fenceless_update r f] is like [update r f] except that in case [f x == x]
      the update may be reordered. *)

  val fenceless_modify :
    ?timeoutf:float -> ?backoff:Backoff.t -> 'a t -> ('a -> 'a) -> unit
  (** [fenceless_modify r f] is like [modify r f] except that in case [f x == x]
      the modify may be reordered. *)
end

(** {1 Manipulating multiple locations atomically}

    Multiple shared memory locations can be manipulated atomically using the
    {!Xt} module to explicitly pass a transaction log to record accesses.

    Atomic operations over multiple shared memory locations are performed in two
    or three phases:

    1. The first phase essentially records a list or log of operations to access
    shared memory locations.  The first phase involves code you write as a user
    of the library.  Aside from some advanced techniques, shared memory
    locations are not mutated during this phase.

    2. The second phase attempts to perform the operations atomically.  This is
    done internally by the library implementation.  Only logically invisible
    writes to shared memory locations are performed during this phase.

    3. In [`Obstruction_free] {{!Mode.t} mode} a third phase verifies all
    read-only operations.  This is also done internally by the library
    implementation.

    Each phase may fail.  In particular, in the first phase, as no changes to
    shared memory have yet been attempted, it is safe, for example, to raise
    exceptions to signal failure.  Failure on the third phase is automatically
    handled by {!Xt.commit}.

    Only after all phases have completed succesfully, the writes to shared
    memory locations are atomically marked as having taken effect and subsequent
    reads of the locations will be able to see the newly written values. *)

(** Explicit transaction log passing on shared memory locations.

    This module provides a way to implement composable transactions over shared
    memory locations.  A transaction is a function written by the library user
    and can be thought of as a specification of a sequence of {!Xt.get} and
    {!Xt.set} accesses to shared memory locations.  To actually perform the
    accesses one then {!Xt.commit}s the transaction.

    Transactions should generally not perform arbitrary side-effects, because
    when a transaction is committed it may be attempted multiple times meaning
    that the side-effects are also performed multiple times.  {!Xt.post_commit}
    can be used to perform an action only once after the transaction has been
    committed succesfully.

    {b WARNING}: To make it clear, the operations provided by the {!Loc} module
    for accessing individual shared memory locations do not implicitly go
    through the transaction mechanism and should generally not be used within
    transactions.  There are advanced algorithms where one might, within a
    transaction, perform operations that do not get recorded into the
    transaction log.  Using such techniques correctly requires expert knowledge
    and is not recommended for casual users.

    As an example, consider an implementation of doubly-linked circular
    lists. Instead of using a mutable field, [ref], or [Atomic.t], one would use
    a shared memory location, or {!Loc.t}, for the pointers in the node type:

    {[
      type 'a node = {
        succ: 'a node Loc.t;
        pred: 'a node Loc.t;
        datum: 'a;
      }
    ]}

    To remove a node safely one wants to atomically update the [succ] and [pred]
    pointers of the predecessor and successor nodes and to also update the
    [succ] and [pred] pointers of a node to point to the node itself, so that
    removal becomes an {{:https://en.wikipedia.org/wiki/Idempotence} idempotent}
    operation.  Using explicit transaction log passing one could implement the
    [remove] operation as follows:

    {[
      let remove ~xt node =
        (* Read pointers to the predecessor and successor nodes: *)
        let pred = Xt.get ~xt node.pred in
        let succ = Xt.get ~xt node.succ in
        (* Update pointers in this node: *)
        Xt.set ~xt node.succ node;
        Xt.set ~xt node.pred node;
        (* Update pointers to this node: *)
        Xt.set ~xt pred.succ succ;
        Xt.set ~xt succ.pred pred
    ]}

    The labeled argument, [~xt], refers to the transaction log. Transactional
    operations like {!Xt.get} and {!Xt.set} are then recorded in that log. To
    actually remove a node, we need to commit the transaction

    {@ocaml skip[
      Xt.commit { tx = remove node }
    ]}

    which repeatedly calls the transaction function, [tx], to record a
    transaction log and attempts to atomically perform it until it succeeds.

    Notice that [remove] is not recursive. It doesn't have to account for
    failure or perform a backoff. It is also not necessary to know or keep track
    of what the previous values of locations were. All of that is taken care of
    for us by the transaction log and the {!Xt.commit} function.  Furthermore,
    [remove] can easily be called as a part of a more complex transaction. *)
module Xt : sig
  type 'x t
  (** Type of an explicit transaction log on shared memory locations.

      Note that a transaction log itself is not safe against concurrent or
      parallel use and should generally only be used by a single thread of
      execution.  If a new thread of execution is spawned inside a function
      recording shared memory accesses to a log and the new thread of execution
      also records accesses to the log it may become inconsistent. *)

  (** {1 Recording accesses}

      Accesses of shared memory locations using an explicit transaction log
      first ensure that the initial value of the shared memory location is
      recorded in the log and then act on the current value of the shared memory
      location as recorded in the log.

      It is important to understand that it is possible for a transaction to
      observe the contents of two (or more) different shared memory locations
      from two (or more) different committed updates.  This means that
      invariants that hold between two (or more) different shared memory
      locations may be seen as broken inside the transaction function.  However,
      it is not possible for the transaction attempt to succeed after it has
      seen such an inconsistent view of the shared memory locations.

      To mitigate potential issues due to this read skew anomaly and due to very
      long running transactions, all of the access recording operations in this
      section periodically validate the entire transaction log.  An important
      guideline for writing transactions is that loops inside a transaction
      should always include an access of some shared memory location through the
      transaction log or should otherwise be guaranteed to be bounded. *)

  val get : xt:'x t -> 'a Loc.t -> 'a
  (** [get ~xt r] returns the current value of the shared memory location [r] in
      the explicit transaction log [xt]. *)

  val set : xt:'x t -> 'a Loc.t -> 'a -> unit
  (** [set ~xt r v] records the current value of the shared memory location [r]
      to be the given value [v] in the explicit transaction log [xt]. *)

  val update : xt:'x t -> 'a Loc.t -> ('a -> 'a) -> 'a
  (** [update ~xt r f] is equivalent to [let x = get ~xt r in set ~xt r (f x); x]
      with the limitation that [f] must not and is not allowed to access the
      transaction log. *)

  val modify : xt:'x t -> 'a Loc.t -> ('a -> 'a) -> unit
  (** [modify ~xt r f] is equivalent to [let x = get ~xt r in set ~xt r (f x)]
      with the limitation that [f] must not and is not allowed to access the
      transaction log. *)

  val exchange : xt:'x t -> 'a Loc.t -> 'a -> 'a
  (** [exchange ~xt r v] is equivalent to [update ~xt r (fun _ -> v)]. *)

  val swap : xt:'x t -> 'a Loc.t -> 'a Loc.t -> unit
  (** [swap ~xt l1 l2] is equivalent to [set ~xt l1 @@ exchange ~xt l2 @@ get ~xt l1]. *)

  val compare_and_set : xt:'x t -> 'a Loc.t -> 'a -> 'a -> bool
  (** [compare_and_set ~xt r before after] is equivalent to
      [compare_and_swap ~xt r before after == before]. *)

  val compare_and_swap : xt:'x t -> 'a Loc.t -> 'a -> 'a -> 'a
  (** [compare_and_swap ~xt r before after] is equivalent to

      {@ocaml skip[
        update ~xt r @@ fun actual ->
        if actual == before then after else actual
      ]} *)

  val fetch_and_add : xt:'x t -> int Loc.t -> int -> int
  (** [fetch_and_add ~xt r n] is equivalent to [update ~xt r ((+) n)]. *)

  val incr : xt:'x t -> int Loc.t -> unit
  (** [incr ~xt r] is equivalent to [fetch_and_add ~xt r 1 |> ignore]. *)

  val decr : xt:'x t -> int Loc.t -> unit
  (** [decr ~xt r] is equivalent to [fetch_and_add ~xt r (-1) |> ignore]. *)

  (** {1 Blocking} *)

  val to_blocking : xt:'x t -> (xt:'x t -> 'a option) -> 'a
  (** [to_blocking ~xt tx] converts the non-blocking transaction [tx] to a
      blocking transaction by retrying on [None]. *)

  val to_nonblocking : xt:'x t -> (xt:'x t -> 'a) -> 'a option
  (** [to_nonblocking ~xt tx] converts the blocking transaction [tx] to a
      non-blocking transaction by returning [None] on retry. *)

  (** {1 Nested transactions}

      The transaction mechanism does not implicitly rollback changes recorded in
      the transaction log.  Using {!snapshot} and {!rollback} it is possible to
      implement nested conditional transactions that may tentatively record
      changes in the transaction log and then later discard those changes. *)

  type 'x snap
  (** Type of a {!snapshot} of a transaction log. *)

  val snapshot : xt:'x t -> 'x snap
  (** [snapshot ~xt] returns a snapshot of the transaction log.

      Taking a snapshot is a fast constant time [O(1)] operation. *)

  val rollback : xt:'x t -> 'x snap -> unit
  (** [rollback ~xt snap] discards any changes of shared memory locations
      recorded in the transaction log after the [snap] was taken by {!snapshot}.

      Performing a rollback is potentially as expensive as linear time [O(n)] in
      the number of locations accessed, but, depending on the exact access
      patterns, may also be performed more quickly.  The implementation is
      optimized with the assumption that a rollback is performed at most once
      per snapshot.

      {b NOTE}: Only changes are discarded.  Any location newly accessed after
      the snapshot was taken will remain recorded in the log as a read-only
      entry. *)

  val first : xt:'x t -> (xt:'x t -> 'a) list -> 'a
  (** [first ~xt txs] calls each transaction in the given list in turn and
      either returns the value returned by the first transaction in the list or
      raises {!Retry.Later} in case all of the transactions raised
      {!Retry.Later}.

      {b NOTE}: [first] does not automatically rollback changes made by the
      transactions. *)

  (** {1 Post commit actions} *)

  val post_commit : xt:'x t -> (unit -> unit) -> unit
  (** [post_commit ~xt action] adds the [action] to be performed after the
      transaction has been committed successfully. *)

  (** {1 Validation} *)

  val validate : xt:'x t -> 'a Loc.t -> unit
  (** [validate ~xt r] determines whether the shared memory location [r] has
      been modified outside of the transaction and raises {!Retry.Invalid} in
      case it has.

      Due to the possibility of read skew, in cases where some important
      invariant should hold between two or more different shared memory
      locations, one may explicitly validate the locations, after reading all of
      them, to ensure that no read skew is possible. *)

  (** {1 Advanced} *)

  val is_in_log : xt:'x t -> 'a Loc.t -> bool
  (** [is_in_log ~xt r] determines whether the shared memory location [r] has
      been accessed by the transaction. *)

  (** {1 Performing accesses} *)

  type 'a tx = { tx : 'x. xt:'x t -> 'a } [@@unboxed]
  (** Type of a transaction function that is polymorphic with respect to an
      explicit transaction log.  The universal quantification helps to ensure
      that the transaction log cannot accidentally escape. *)

  val call : xt:'x t -> 'a tx -> 'a
  (** [call ~xt tx] is equivalent to [tx.Xt.tx ~xt]. *)

  val commit :
    ?timeoutf:float -> ?backoff:Backoff.t -> ?mode:Mode.t -> 'a tx -> 'a
  (** [commit tx] repeatedly calls [tx] to record a log of shared memory
      accesses and attempts to perform them atomically until it succeeds and
      then returns whatever [tx] returned.  [tx] may raise {!Retry.Later} or
      {!Retry.Invalid} to explicitly request a retry or any other exception to
      abort the transaction.

      The default {{!Mode.t} [mode]} for [commit] is [`Obstruction_free].
      However, after enough attempts have failed during the verification step,
      [commit] switches to [`Lock_free]. *)

  (**/**)

  val unsafe_modify : xt:'x t -> 'a Loc.t -> ('a -> 'a) -> unit
  (** [unsafe_modify ~xt r f] is equivalent to [modify ~xt r f], but does not
      assert against misuse. *)

  val unsafe_update : xt:'x t -> 'a Loc.t -> ('a -> 'a) -> 'a
  (** [unsafe_update ~xt r f] is equivalent to [update ~xt r f], but does not
      assert against misuse. *)
end
