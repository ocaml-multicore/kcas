(** {1 Auxiliary modules} *)

module Backoff : module type of Backoff
(** Randomized exponential backoff mechanism. *)

(** {1 Individual locations}

    Individual shared memory locations can be manipulated through the {!Loc}
    module that is essentially compatible with the Stdlib [Atomic] module. *)

(** Shared memory locations. *)
module Loc : sig
  type !'a t
  (** Type of shared memory locations. *)

  val make : 'a -> 'a t
  (** [make initial] creates a new shared memory location with the [initial]
      value. *)

  val get_id : 'a t -> int
  (** [get_id r] returns the unique id of the shared memory location [r]. *)

  val get : 'a t -> 'a
  (** [get r] reads the current value of the shared memory location [r]. *)

  val compare_and_set : 'a t -> 'a -> 'a -> bool
  (** [compare_and_set r before after] atomically updates the shared memory
      location [r] to the [after] value if the current value of [r] is the
      [before] value. *)

  val update : ?backoff:Backoff.t -> 'a t -> ('a -> 'a) -> 'a
  (** [update r f] repeats [let b = get r in compare_and_set r b (f b)] until it
      succeeds and then returns the [b] value.  It is safe for the given
      function [f] to raise an exception to abort the update. *)

  val modify : ?backoff:Backoff.t -> 'a t -> ('a -> 'a) -> unit
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
end

(** {1 Manipulating multiple locations atomically}

    Multiple shared memory locations can be manipulated atomically using either

    - {!Op}, to specify a list of primitive operations to perform, or
    - {!Xt}, to explicitly pass a transaction log to record accesses.

    Atomic operations over multiple shared memory locations are performed in two
    or three phases:

    1. The first phase essentially records a list or log of operations to access
    shared memory locations.

    2. The second phase attempts to perform the operations atomically.

    3. In {!Mode.obstruction_free} a third phase verifies all read-only
    operations.

    Each phase may fail.  In particular, in the first phase, as no changes to
    shared memory have yet been attempted, it is safe, for example, to raise
    exceptions to signal failure.  Failure on the third phase raises
    {!Mode.Interference}. *)

(** Operating modes of the [k-CAS-n-CMP] algorithm. *)
module Mode : sig
  type t
  (** Type of an operating mode of the [k-CAS-n-CMP] algorithm. *)

  val lock_free : t
  (** In [lock_free] mode the algorithm makes sure that at least one domain will
      be able to make progress by performing read-only operations as read-write
      operations. *)

  val obstruction_free : t
  (** In [obstruction_free] mode the algorithm proceeds optimistically and
      allows read-only operations to fail due to {!Interference} from other
      domains that might have been prevented in the {!lock_free} mode. *)

  exception Interference
  (** Exception raised when interference from other domains is detected in the
      {!obstruction_free} mode.  Interference may happen when some location is
      accessed by both a compare-and-set and a (read-only) compare operation.
      It is not necessary for the compare-and-set to actually change the logical
      value of the location. *)
end

(** Operations on shared memory locations. *)
module Op : sig
  type t
  (** Type of operations on shared memory locations. *)

  val make_cas : 'a Loc.t -> 'a -> 'a -> t
  (** [make_cas r before after] is an operation that attempts to set the shared
      memory location [r] to the [after] value and succeeds if the current
      content of [r] is the [before] value. *)

  val make_cmp : 'a Loc.t -> 'a -> t
  (** [make_cmp r expected] is an operation that succeeds if the current value
      of the shared memory location [r] is the [expected] value. *)

  val get_id : t -> int
  (** [get_id op] returns the unique id of the shared memory reference targeted
      by the [op]eration. *)

  val is_on_loc : t -> 'a Loc.t -> bool
  (** [is_on_loc op r] determines whether the target of [op] is the shared
      memory location [r]. *)

  val atomic : t -> bool
  (** [atomic op] attempts to perform the given operation atomically.  Returns
      [true] on success and [false] on failure. *)

  val atomically : ?mode:Mode.t -> t list -> bool
  (** [atomically ops] attempts to perform the given operations atomically.  If
      used in {!Mode.obstruction_free} may raise {!Mode.Interference}.
      Otherwise returns [true] on success and [false] on failure.  The default
      for [atomically] is {!Mode.lock_free}.

      The algorithm requires provided operations to follow a global total order.
      To eliminate a class of bugs, the operations are sorted automatically.  If
      the operations are given in either ascending or descending order of the
      targeted shared memory location ids, then sorting is done in linear time
      [O(n)] and does not increase the time complexity of the algorithm.
      Otherwise sorting may take linearithmic time [O(n*log(n))]. *)
end

(** {2 Composable transactions on multiple locations}

    The {!Xt} module provides a way to implement composable transactions over
    shared memory locations.  A transaction can be thought of as a specification
    of a sequence of {!Xt.get} and {!Xt.set} accesses to shared memory
    locations.  To actually perform the accesses one then {!Xt.commit}s the
    transaction.

    {b WARNING}: Operations provided by the {!Loc} module for accessing
    individual shared memory locations are not transactional.  There are cases
    where it can be advantageous to perform operations along with a transaction
    that do not get recorded into the transaction log, but doing so requires one
    to reason about the potential parallel interleavings of operations.

    Transactions should also generally not perform arbitrary side-effects,
    because when a transaction is committed it may be attempted multiple times
    meaning that the side-effects are also performed multiple times. *)

(** Explicit transaction log passing on shared memory locations. *)
module Xt : sig
  type 'x t
  (** Type of an explicit transaction log on shared memory locations.

      Note that a transaction log itself is not domain-safe and should generally
      only be used by a single domain.  If a new domain is spawned inside a
      function recording shared memory accesses to a log and the new domain also
      records accesses to the log it may become inconsistent. *)

  (** {1 Recording accesses}

      Accesses of shared memory locations using an explicit transaction log
      first ensure that the initial value of the shared memory location is
      recorded in the log and then act on the current value of the shared memory
      location as recorded in the log. *)

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

  val compare_and_swap : xt:'x t -> 'a Loc.t -> 'a -> 'a -> 'a
  (** [compare_and_swap ~xt r before after] is equivalent to

      {[
        update ~xt r @@ fun actual ->
        if actual == before then after else actual
      ]} *)

  val fetch_and_add : xt:'c t -> int Loc.t -> int -> int
  (** [fetch_and_add ~xt r n] is equivalent to [update ~xt r ((+) n)]. *)

  val incr : xt:'x t -> int Loc.t -> unit
  (** [incr ~xt r] is equivalent to [fetch_and_add ~xt r 1 |> ignore]. *)

  val decr : xt:'x t -> int Loc.t -> unit
  (** [decr ~xt r] is equivalent to [fetch_and_add ~xt r (-1) |> ignore]. *)

  (** {1 Post commit actions} *)

  val post_commit : xt:'x t -> (unit -> unit) -> unit
  (** [post_commit ~xt action] adds the [action] to be performed after the
      transaction has been performed successfully. *)

  (** {1 Advanced} *)

  val is_in_log : xt:'x t -> 'a Loc.t -> bool
  (** [is_in_log ~xt r] determines whether the shared memory location [r] has
      been accessed by the transaction. *)

  (** {1 Performing accesses} *)

  type 'a tx = { tx : 'x. xt:'x t -> 'a } [@@unboxed]
  (** Type of a transaction function that is polymorphic with respect to an
      explicit transaction log.  The universal quantification helps to ensure
      that the transaction log cannot accidentally escape. *)

  val call : 'a tx -> xt:'x t -> 'a
  (** [call ~xt tx] is equivalent to [tx.Xt.tx ~xt]. *)

  val attempt : ?mode:Mode.t -> 'a tx -> 'a
  (** [attempt tx] attempts to atomically perform the transaction over shared
      memory locations recorded by calling [tx] with a fresh explicit
      transaction log.  If used in {!Mode.obstruction_free} may raise
      {!Mode.Interference}.  Otherwise either raises [Exit] on failure to commit
      the transaction or returns the result of the transaction.  The default for
      [attempt] is {!Mode.lock_free}. *)

  val commit : ?backoff:Backoff.t -> ?mode:Mode.t -> 'a tx -> 'a
  (** [commit tx] repeats [attempt tx] until it does not raise [Exit] or
      {!Mode.Interference} and then either returns or raises whatever attempt
      returned or raised.

      The default for [commit] is {!Mode.obstruction_free}.  However, after
      enough attempts have failed during the verification step, [commit]
      switches to {!Mode.lock_free}.

      Note that, aside from using exponential backoff to reduce contention, the
      transaction mechanism has no way to intelligently wait until shared memory
      locations are modified by other domains. *)
end
