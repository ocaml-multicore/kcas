(** {1 Auxiliary modules} *)

module Backoff : module type of Backoff
(** Randomized exponential backoff mechanism. *)

(** {1 Individual locations}

    Individual shared memory locations can be manipulated through the {!Loc}
    module that is essentially compatible with the Stdlib [Atomic] module. *)

(** Shared memory locations. *)
module Loc : sig
  type 'a t
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

    - {!Op}, to specify a list of primitive operations to perform,
    - {!Tx}, to specify a composable transaction, or
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

    The {!Tx} and {!Xt} modules provide two different ways to implement
    composable transactions over shared memory locations.  Using either of those
    one essentially declares a transaction that specifies how to access shared
    memory locations.  To actually perform the accesses one then separately
    commits the transaction.

    Accesses of shared memory locations inside either form of transaction should
    generally use the access operations, such as {!Tx.get} and {!Xt.get},
    provided by the corresponding module.  Transactions should also generally
    not perform arbitrary side-effects, because when a transaction is committed
    it may be attempted multiple times meaning that the side-effects are also
    performed multiple times.

    {b WARNING}: Operations provided by the {!Loc} module for accessing
    individual shared memory locations are not transactional.  There are cases
    where it can be advantageous to perform operations along with a transaction
    that do not get recorded into the transaction log, but doing so requires one
    to reason about the potential parallel interleavings of operations. *)

(** Transactions on shared memory locations. *)
module Tx : sig
  type 'a t
  (** Type of transactions on shared memory locations.

      A transaction can be thought of as a specification of a sequence of {!get}
      and {!set} accesses to shared memory locations that can be {!attempt}ed to
      perform the accesses atomically.

      Transactions can be composed both sequentially (see {!let*}) and
      conditionally (see {!(<|>)} and {!forget}).

      Here is an example of unconditionally {!commit}ting a transaction that
      swaps the values of the two shared memory locations [x_loc] and [y_loc]
      and returns their sum:

      {[
        commit begin
          let* x = get x_loc
          and* y = get y_loc in
          let+ () = set y_loc x
          and+ () = set x_loc y in
          x + y
        end
      ]}

      Above, [get y_loc] and [set y_loc x] are intentionally one after the
      other, because the internal log used within transactions to record {!get}s
      and {!set}s is optimized for accessing recently accessed elements again.

      Here is an example of {!attempt}ing a conditional transaction

      {[
        attempt begin
          begin
            let* x = get x_loc in
            if x < 0 then
              forget
            else
              return x
          end
          <|> get y_loc
        end
      ]}

      that reads [x_loc], but {!forget}s the access in case [x_loc] had a
      negative value and then reads [y_loc] instead.

      What does that actually mean?  Let's assume [x_loc] initially contains a
      negative value.  During the first phase of the transaction [x_loc] is read
      and is found to be negative.  The access is forgotten.  Let's assume that
      after that the value of [x_loc] is modified by some other domain.  The
      transaction continues to read [y_loc] and results in the value of [y_loc].
      The transaction is allowed to commit despite the value of [x_loc] having
      been changed during the transaction.

      Consider the following similar looking conditional transaction {!attempt}:

      {[
        attempt begin
          let* x = get x_loc in
          if x < 0 then
            get y_loc
          else
            return x
        end
      ]}

      If some other domain modifies [x_loc] after it has been read by the above
      transaction attempt, then the commit phase will fail, because the access
      of [x_loc] is found to be inconsistent.

      Note that neither of the above conditional examples is generally
      preferred, because they have fundamentally different semantics and the
      choice of whether accesses should, should not, or may be safely forgotten
      depends on what the desired semantics are for the transaction. *)

  (** {1 Access combinators} *)

  val get : 'a Loc.t -> 'a t
  (** [get r] accesses the shared memory location [r] inside the transaction
      and results in the current value of [r] inside the transaction. *)

  val get_as : ('a -> 'b) -> 'a Loc.t -> 'b t
  (** [get_as g r] is equivalent to [get r |> map g]. *)

  val set : 'a Loc.t -> 'a -> unit t
  (** [set r v] accesses the shared memory location [r] inside the transaction
      and sets the current value of [r] to the value [v] inside the
      transaction. *)

  val update : 'a Loc.t -> ('a -> 'a) -> 'a t
  (** [update r f] is equivalent to [let* x = get r in let+ () = set r (f x) in x]. *)

  val update_as : ('a -> 'b) -> 'a Loc.t -> ('a -> 'a) -> 'b t
  (** [update_as g r f] is equivalent to [update r f |> map g]. *)

  val modify : 'a Loc.t -> ('a -> 'a) -> unit t
  (** [modify r f] is equivalent to [let* x = get r in set r (f x)]. *)

  val exchange : 'a Loc.t -> 'a -> 'a t
  (** [exchange r v] is equivalent to [update r (fun _ -> v)]. *)

  val exchange_as : ('a -> 'b) -> 'a Loc.t -> 'a -> 'b t
  (** [exchange_as g r v] is equivalent to [update r (fun _ -> v) |> map g]. *)

  val compare_and_swap : 'a Loc.t -> 'a -> 'a -> 'a t
  (** [compare_and_swap r before after] is equivalent to

      {[
        update r @@ fun actual ->
        if actual == before then after else actual
      ]} *)

  val fetch_and_add : int Loc.t -> int -> int t
  (** [fetch_and_add r n] is equivalent to [update r ((+) n)]. *)

  val incr : int Loc.t -> unit t
  (** [incr r] is equivalent to [modify r ((+) 1)]. *)

  val decr : int Loc.t -> unit t
  (** [decr r] is equivalent to [modify r ((+) (-1))]. *)

  (** {1 Sequencing combinators} *)

  val return : 'a -> 'a t
  (** [return v] is a transactional operation whose result is the value [v]. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [let* x = x_tx in to_y_tx x] is the sequential composition of the
      transaction [x_tx] with the transaction [to_y_tx x] computed from the
      result [x] of [x_tx]. *)

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  (** [( and* ) x_tx tx_y] is a transaction that performs both the transaction
      [x_tx] and the transaction [tx_y] and returns a pair of their results. *)

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  (** [let+ x = tx in fn x] is equivalent to [let* x = tx in return (fn x)]. *)

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  (** [and+] is a synonym for {!and*} for use with {!let+}. *)

  val ( >> ) : 'ignore t -> 'a t -> 'a t
  (** [u_tx >> x_tx] is equivalent to [let* _ = u_tx in x_tx]. *)

  val ( >>. ) : 'ignore t -> 'a -> 'a t
  (** [u_tx >>. x] is equivalent to [let+ _ = u_tx in x]. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [>>=] is a synonym for {!let*}. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map xy x_tx] is equivalent to [let+ x = x_tx in xy x]. *)

  val delay : (unit -> 'a t) -> 'a t
  (** [delay to_tx] is equivalent to [let* () = return () in to_tx ()] *)

  val try_in : (exn -> 'b t) -> ('a -> 'b t) -> 'a t -> 'b t
  (** [try_in e_to_y_tx x_to_y_tx x_tx] is for handling exceptions during the
      access log recording phase of running a transaction.  In case the
      transaction [x_tx] raises during the first phase, the accesses from [x_tx]
      are forgotten.

      [try_in e_to_y_tx x_to_y_tx x_tx] is the equivalent of

      {[
        match run_phase_1 x_tx with
        | x -> run_phase_1 (x_to_y_tx x)
        | exception e -> run_phase_1 (e_to_y_tx e)
      ]}

      for transactions.

      Note that the order of parameters is chosen with the following style in
      mind:

      {[
        transaction
        |> try_in (fun exn -> (* handle failure *)) @@ fun value ->
           (* continue successfully *)
      ]} *)

  (** {1 Conditional transactions} *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** [l_tx <|> r_tx] is a left-biased choice between the two transactions
      [l_tx] and [r_tx].

      If the [l_tx] transaction successfully produces a log of operations in the
      first phase of {!attempt}, then those are attempted in the second phase.
      On the other hand, if the [l_tx] transaction raises [Exit], then the
      choice acts like [r_tx].

      For example, [(set x_loc x >> forget) <|> r_tx] is equivalent to [r_tx]
      meaning that the [set x_loc x] access will not be part of the operations
      attempted in the second phase of a transaction. *)

  val forget : 'a t
  (** [forget] is equivalent to [delay (fun () -> raise Exit)].

      The name [forget] was chosen to give the idea that this causes the
      transaction to forget any accesses made during the forgotten part of a
      transaction. *)

  (** {1 Performing transactions} *)

  val attempt : ?mode:Mode.t -> 'a t -> 'a
  (** [attempt tx] attempts to atomically perform the given transaction over
      shared memory locations.  If used in {!Mode.obstruction_free} may raise
      {!Mode.Interference}.  Otherwise either raises [Exit] on failure to commit
      the transaction or returns the result of the transaction.  The default for
      [attempt] is {!Mode.lock_free}. *)

  val commit : ?backoff:Backoff.t -> ?mode:Mode.t -> 'a t -> 'a
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
      with the limitation that [f] must not and is not allowed to record
      accesses to the transaction log. *)

  val modify : xt:'x t -> 'a Loc.t -> ('a -> 'a) -> unit
  (** [modify ~xt r f] is equivalent to [let x = get ~xt r in set ~xt r (f x)]
      with the limitation that [f] must not and is not allowed to record
      accesses to the transaction log. *)

  val exchange : xt:'x t -> 'a Loc.t -> 'a -> 'a
  (** [exchange ~xt r v] is equivalent to [update ~xt r (fun _ -> v)]. *)

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

  (** {1 Conversions} *)

  val of_tx : 'a Tx.t -> xt:'x t -> 'a
  (** [of_tx tx] converts the given {!Tx} transaction [tx] to an explicit log
      passing function. *)

  val to_tx : 'a tx -> 'a Tx.t
  (** [to_tx tx] converts the given explicit log passing function [tx] to a
      {!Tx} transaction. *)
end
