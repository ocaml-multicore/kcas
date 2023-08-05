(** This is a library of compositional lock-free data structures and primitives
    for communication and synchronization implemented using {!Kcas}.

    All data structure implementations in this library are concurrency and
    parallelism safe and should strive to provide the following guarantees:

    - Provided operations are {i strictly serializable} (i.e. both
      {{:https://en.wikipedia.org/wiki/Linearizability}linerizable} and
      {{:https://en.wikipedia.org/wiki/Serializability}serializable}).
    - Provided operations are efficient, either
      ({{:https://en.wikipedia.org/wiki/Amortized_analysis}amortized}) constant
      time, [O(1)], or logarithmic time, [O(log(n))].
    - Provided operations are
      {{:https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom}lock-free}
      and designed to avoid
      {{:https://en.wikipedia.org/wiki/Starvation_(computer_science)}starvation}
      under moderate contention.
    - Provided read-only operations scale perfectly when only read-only
      operations are performed in parallel.

    Unobvious exceptions to the above guarantees should be clearly and
    explicitly documented.

    The main feature of these data structure implementations is their
    compositionality.  If your application does not need compositionality, then
    other concurrency and parallelism safe data structure libraries may
    potentially offer better performance.

    But why should you care about composability?

    As an example, consider the implementation of a least-recently-used (LRU)
    cache or a bounded associative map. A simple sequential approach to
    implement a LRU cache is to use a hash table and a doubly-linked list and
    keep track of the amount of space in the cache:

    {[
      type ('k, 'v) cache =
        { space: int Loc.t;
          table: ('k, 'k Dllist.node * 'v) Hashtbl.t;
          order: 'k Dllist.t }
    ]}

    On a cache lookup the doubly-linked list node corresponding to the accessed
    key is moved to the left end of the list:

    {[
      let get_opt {table; order; _} key =
        Hashtbl.find_opt table key
        |> Option.map @@ fun (node, datum) ->
           Dllist.move_l node order; datum
    ]}

    On a cache update, in case of overflow, the association corresponding to the
    node on the right end of the list is dropped:

    {[
      let set {table; order; space; _} key datum =
        let node =
          match Hashtbl.find_opt table key with
          | None ->
            if 0 = Loc.update space (fun n -> max 0 (n-1))
            then Dllist.take_opt_r order
                 |> Option.iter (Hashtbl.remove table);
            Dllist.add_l key order
          | Some (node, _) -> Dllist.move_l node order; node
        in
        Hashtbl.replace table key (node, datum)
    ]}

    Sequential algorithms such as the above are so common that one does not even
    think about them. Unfortunately, in a concurrent setting the above doesn't
    work even if the individual operations on lists and hash tables were atomic
    as they are in this library.

    But how would one make the operations on a cache atomic as a whole? As
    explained by Maurice Herlihy in one of his talks on
    {{:https://youtu.be/ZkUrl8BZHjk?t=1503} Transactional Memory} adding locks
    to protect the atomicity of the operation is far from trivial.

    Fortunately, rather than having to e.g. wrap the cache implementation behind
    a {{:https://en.wikipedia.org/wiki/Lock_(computer_science)} mutex} and make
    another individually atomic yet uncomposable data structure, or having to
    learn a completely different programming model and rewrite the cache
    implementation, we can use the transactional programming model provided by
    the {!Kcas} library and the transactional data structures provided by this
    library to trivially convert the previous implementation to a lock-free
    composable transactional data structure.

    To make it so, we simply use transactional versions, [*.Xt.*], of operations
    on the data structures and explicitly pass a transaction log, [~xt], to the
    operations. For the [get_opt] operation we end up with

    {[
      let get_opt ~xt {table; order; _} key =
        Hashtbl.Xt.find_opt ~xt table key
        |> Option.map @@ fun (node, datum) ->
           Dllist.Xt.move_l ~xt node order; datum
    ]}

    and the [set] operation is just as easy to convert to a transactional
    version.  One way to think about transactions is that they give us back the
    ability to compose programs such as the above. *)

(** {1 [Stdlib] style data structures}

    The data structures in this section are designed to closely mimic the
    corresponding unsynchronized data structures in the OCaml [Stdlib].  Each of
    these provide a non-compositional, but concurrency and parallelism safe,
    interface that is close to the [Stdlib] equivalent.  Additionally,
    compositional transactional interfaces are provided for some operations.

    These implementations will use more space than the corresponding [Stdlib]
    data structures.  Performance, when accessed concurrently, should be
    competitive or superior compared to naïve locking. *)

module Hashtbl = Hashtbl
module Queue = Queue
module Stack = Stack

(** {1 Communication and synchronization primitives}  *)

module Mvar = Mvar
module Promise = Promise

(** {1 Linked data structures} *)

module Dllist = Dllist

(** {1 Utilities} *)

module Accumulator = Accumulator
