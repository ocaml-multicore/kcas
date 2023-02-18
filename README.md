[API reference](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/index.html)
&middot; <sub><sup>(The API was redesigned in version 0.2.0. See
[API reference for version 0.1.8](https://ocaml-multicore.github.io/kcas/0.1.8/kcas/Kcas/index.html).)</sup></sub>

# **kcas** &mdash; Multi-word compare-and-swap library

**kcas** provides an implementation of atomic
[lock-free](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom)
multi-word [compare-and-swap](https://en.wikipedia.org/wiki/Compare-and-swap)
(MCAS), which is a powerful tool for designing concurrent algorithms.

Features and properties:

- **_Efficient_**: In the common uncontended case only **k + 1** single-word
  CASes are required per k-CAS.

- **_Lock-free_**: The underlying algorithm guarantees that at least one domain
  will be able to make progress.

- **_Disjoint-access parallel_**: Unrelated operations progress independently,
  without interference, even if they occur at the same time.

- **_Read-only compares_**: The algorithm supports
  [obstruction-free](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Obstruction-freedom)
  read-only compare (CMP) operations that can be performed on overlapping
  locations in parallel without interference.

- **_Composable_**: Independently developed transactions can be composed with
  ease.

**kcas** is [published on **opam**](https://opam.ocaml.org/packages/kcas/) and
is distributed under the [ISC license](LICENSE.md).

## Contents

- [A quick tour](#a-quick-tour)
- [Introduction](#introduction)
  - [Creating and manipulating individual shared memory locations](#creating-and-manipulating-individual-shared-memory-locations)
  - [Programming with primitive operations](#programming-with-primitive-operations)
  - [Programming with transactions](#programming-with-transactions)
    - [A transactional lock-free stack](#a-transactional-lock-free-stack)
    - [A transactional lock-free queue](#a-transactional-lock-free-queue)
    - [Composing transactions](#composing-transactions)
    - [About transactions](#about-transactions)
  - [Programming with explicit transaction log passing](#programming-with-explicit-transaction-log-passing)
    - [A transactional lock-free leftist heap](#a-transactional-lock-free-leftist-heap)
- [Development](#development)

## A quick tour

To use the library

```ocaml
# #require "kcas"
# open Kcas
```

one first creates shared memory locations:

```ocaml
# let a = Loc.make 0
  and b = Loc.make 0
  and x = Loc.make 0
val a : int Loc.t = <abstr>
val b : int Loc.t = <abstr>
val x : int Loc.t = <abstr>
```

One can then manipulate the locations individually:

```ocaml
# Loc.set a 6
- : unit = ()
# Loc.get a
- : int = 6
```

Attempt primitive operations over multiple locations:

```ocaml
# Op.atomically [
    Op.make_cas a 6 10;
    Op.make_cas b 0 52
  ]
- : bool = true
```

Perform transactions over them:

```ocaml
# Tx.(
    commit (
      let* a = get a
      and* b = get b in
      set x (b - a)
    )
  )
- : unit = ()
```

Explicitly pass a transaction log through a computation:

```ocaml
# Xt.commit { tx = fun ~xt ->
    Xt.set ~xt a (Xt.get ~xt b) }
- : unit = ()
```

And get the answer:

```ocaml
# Loc.get x
- : int = 42
```

## Introduction

The API of **kcas** is divided into submodules. The main modules are

- [`Loc`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html),
  providing an abstraction of _shared memory locations_,

- [`Op`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html),
  providing an interface for _primitive operations_ over multiple shared memory
  locations,

- [`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html),
  providing _composable transactions_ over shared memory locations, and

- [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html),
  providing _explicit transaction log passing_ over shared memory locations.

The following sections discuss each of the above in turn.

### Creating and manipulating individual shared memory locations

The [`Loc`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html)
module is essentially compatible with the Stdlib
[`Atomic`](https://v2.ocaml.org/api/Atomic.html) module, except that a number of
functions take an optional
[`backoff`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Backoff/index.html)
as an argument.

In other words, an application that uses
[`Atomic`](https://v2.ocaml.org/api/Atomic.html), but then needs to perform
atomic operations over multiple atomic locations, could theoretically just
rebind `module Atomic = Loc` and then use the
[`Op`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html),
[`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html),
and/or
[`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html) APIs
to perform operations over multiple locations. This should not be done
just-in-case, however, as, even though **kcas** is efficient, it does naturally
have higher overhead than the Stdlib
[`Atomic`](https://v2.ocaml.org/api/Atomic.html).

### Programming with primitive operations

The [`Op`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html)
module is probably most suitable when using **kcas** as a means to design and
implement new lock-free algorithms.

To program with primitive operations one simply makes a list of CAS operations
using
[`make_cas`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html#val-make_cas)
and then attempts them using
[`atomically`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html#val-atomically).
Typically that needs to be done inside a loop of some kind as such an attempt
can naturally fail.

Let's first
[`make`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-make)
two locations representing stacks:

```ocaml
# let stack_a = Loc.make [19]
  and stack_b = Loc.make [76]
val stack_a : int list Loc.t = <abstr>
val stack_b : int list Loc.t = <abstr>
```

Here is a function that can atomically move an element from given `source` stack
to the given `target` stack:

```ocaml
# let rec move ?(backoff = Backoff.default)
               source
               target =
    match Loc.get source with
    | [] -> raise Exit
    | (elem::rest) as old_source ->
      let old_target = Loc.get target in
      let ops = [
        Op.make_cas source old_source rest;
        Op.make_cas target old_target (elem::old_target)
      ] in
      if not (Op.atomically ops) then
        let backoff = Backoff.once backoff in
        move ~backoff source target
val move : ?backoff:Backoff.t -> 'a list Loc.t -> 'a list Loc.t -> unit =
  <fun>
```

Note that we also used the
[`Backoff`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Backoff/index.html)
module provided by **kcas** above.

Now we can simply call `move`:

```ocaml
# move stack_a stack_b
- : unit = ()
# Loc.get stack_a
- : int list = []
# Loc.get stack_b
- : int list = [19; 76]
```

As one can see, the API provided by
[`Op`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html) is
quite low-level and is not intended for application level programming.

### Programming with transactions

The [`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html)
module provides a higher-level API that is intended to be suitable for both
designing and implementing new lock-free algorithms and as an application level
programming interface for compositional use of such algorithms.

#### A transactional lock-free stack

As our first example of using transactions, let's implement a lock-free stack. A
stack can be just a shared memory location that holds a list of elements:

```ocaml
# type 'a stack = 'a list Loc.t
type 'a stack = 'a list Loc.t
```

To create a stack we just
[`make`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-make)
a new location with an empty list:

```ocaml
# let stack () : _ stack = Loc.make []
val stack : unit -> 'a stack = <fun>
```

To push an element to a stack we
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-modify)
the stack to cons the element onto the list:

```ocaml
# let push stack element =
    Tx.modify stack @@ List.cons element
val push : 'a list Loc.t -> 'a -> unit Tx.t = <fun>
```

Popping an element from a stack is a little more complicated as we need to
handle the case of an empty stack. Let's go with a basic approach where we first
[`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-get)
the content of the stack,
[`set`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-set)
it if necessary, and
[`return`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-return)
an optional element.

```ocaml
# let try_pop stack = Tx.(
    let* content = get stack in
    match content with
    | [] -> return None
    | element :: rest ->
      let+ () = set stack rest in
      Some element
  )
val try_pop : 'a list Loc.t -> 'a option Tx.t = <fun>
```

Above we used the
[`let*`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-let*)
and
[`let+`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-let+)
[binding operators](https://v2.ocaml.org/manual/bindingops.html) to sequence
primitive transactions. We could also implement `try_pop` more concisely using
the infix operators
[`>>=`](<https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-(%3E%3E=)>)
and
[`>>.`](<https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-(%3E%3E.)>):

```ocaml
# let try_pop stack = Tx.(
    get stack >>= function
    | [] -> return None
    | element :: rest ->
      set stack rest >>.
      Some element
  )
val try_pop : 'a list Loc.t -> 'a option Tx.t = <fun>
```

With a couple of useful list manipulation helper functions

```ocaml
# let hd_opt = function
    | [] -> None
    | element :: _ -> Some element
val hd_opt : 'a list -> 'a option = <fun>
# let tl_safe = function
    | [] -> []
    | _ :: rest -> rest
val tl_safe : 'a list -> 'a list = <fun>
```

an even more concise implementation is possible using
[`update_as`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-update_as):

```ocaml
# let try_pop stack = Tx.update_as hd_opt stack tl_safe
val try_pop : 'a list Loc.t -> 'a option Tx.t = <fun>
```

Above,
[`update_as`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-update_as)
is used as a shorthand to both compute the result and the new value for the
stack contents.

If the stack already contained an empty list, `[]`, all of the above variations
of `try_pop` generate a read-only CMP operation in the
[`obstruction_free`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Mode/index.html#val-obstruction_free)
mode. This means that multiple domains may run `try_pop` on an empty stack in
parallel without interference. The variation using
[`update_as`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-update_as)
also makes only a single access to the underlying transaction log and is likely
to be the fastest variation.

So, to use a stack, we first need to create it and then we may
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-commit)
transactions to `push` and `try_pop` elements:

```ocaml
# let a_stack : int stack = stack ()
val a_stack : int stack = <abstr>
# Tx.commit @@ push a_stack 101
- : unit = ()
# Tx.commit @@ try_pop a_stack
- : int option = Some 101
# Tx.commit @@ try_pop a_stack
- : int option = None
```

As an astute reader you may wonder why `push` and `try_pop` return transactions
that we then need to separately
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-commit)
to. We'll get to that soon!

#### A transactional lock-free queue

Let's then implement a lock-free queue. To keep things simple we just use the
traditional two-stack queue data structure:

```ocaml
# type 'a queue = {
    front: 'a list Loc.t;
    back: 'a list Loc.t
  }
type 'a queue = { front : 'a list Loc.t; back : 'a list Loc.t; }
```

To create a queue we
[`make`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-make)
the two locations:

```ocaml
# let queue () = {
    front = Loc.make [];
    back = Loc.make []
  }
val queue : unit -> 'a queue = <fun>
```

To enqueue we just
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-modify)
the back of the queue and `cons` the element to the list:

```ocaml
# let enqueue queue element =
    Tx.modify queue.back @@ List.cons element
val enqueue : 'a queue -> 'a -> unit Tx.t = <fun>
```

Dequeue is again more complicated. First we examine the front of the queue. If
there is an element, we update the front and return the element. If the front is
empty, we examine the back of the queue in `rev`erse. If there is an element we
clear the back, move the rest of the elements to the front, and return the
element. Otherwise we return `None` as the queue was empty.

```ocaml
# let try_dequeue queue = Tx.(
    update queue.front tl_safe >>= function
    | element :: _ -> return (Some element)
    | [] ->
      exchange_as List.rev queue.back [] >>= function
      | [] -> return None
      | element :: rest ->
        set queue.front rest >>.
        Some element
  )
val try_dequeue : 'a queue -> 'a option Tx.t = <fun>
```

Above,
[`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-update)
and
[`exchange_as`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-exchange_as)
are used as convenient shorthands and to reduce the number of accesses to the
transaction log. If both the front and back locations already contained an empty
list, `[]`, the above generates read-only CMP operations in the
[`obstruction_free`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Mode/index.html#val-obstruction_free)
mode allowing multiple domains to run `try_dequeue` on an empty queue in
parallel without interference. Additionally, if the back contained only one
element, no write to the front is generated.

> **_Question_**: _When does a transaction generate a read-only compare against
> a particular location?_
>
> First of all, the transaction must be attempted in the
> [`obstruction_free`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Mode/index.html#val-obstruction_free)
> mode, which is the default mode that
> [`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-commit)
> uses initially.
>
> Additionally, there must be no operation in the transaction that sets a new
> value to the location.
>
> If an operation sets a location to a new value, the full original state of the
> location is forgotten, and the transaction will then later attempt a
> compare-and-set operation against that location even if a later operation
> inside the transaction sets the location to its original value.
>
> The intention behind this approach is to strike a balance between adding
> overhead and also supporting convenient read-only updates.

So, to use a queue, we first need to create it and then we may
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-commit)
transactions to `enqueue` and `try_dequeue` elements:

```ocaml
# let a_queue : int queue = queue ()
val a_queue : int queue = {front = <abstr>; back = <abstr>}
# Tx.commit @@ enqueue a_queue 76
- : unit = ()
# Tx.commit @@ try_dequeue a_queue
- : int option = Some 76
# Tx.commit @@ try_dequeue a_queue
- : int option = None
```

> **_Beware_**: Using two stacks for a queue is easy to implement and performs
> well in many cases. Unfortunately it has one major weakness. The problem is
> that it may take a relatively long time to reverse the back of a queue. This
> can cause
> [starvation](<https://en.wikipedia.org/wiki/Starvation_(computer_science)>) as
> producers may then be able to always complete their transactions before
> consumers and the back of the queue might grow without bound. _Can you see a
> way to avoid this problem?_

#### Composing transactions

The main benefit of the
[`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html) API
over the
[`Op`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html) API
is that transactions are composable. In fact, we already used
[`let*`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-let*)
to compose primitive transactions when implementing transactional stacks and
queues. Composition is not limited to primitive transactions.

For example, one can push multiple elements to a transactional stack atomically:

```ocaml
# Tx.(
    commit (
      push a_stack 3 >>
      push a_stack 1 >>
      push a_stack 4
    )
  )
- : unit = ()
```

Or transfer elements between different transactional data structures:

```ocaml
# Tx.(
    commit (
      try_pop a_stack >>= function
      | Some element ->
        enqueue a_queue element
      | None ->
        return ()
    )
  )
- : unit = ()
```

The ability to compose transactions allows algorithms and data-structures to be
used for a wider variety of purposes.

#### About transactions

The transaction mechanism provided by **kcas** is quite intentionally designed
to be very simple and efficient. This also means that it cannot provide certain
features, because adding such features would either add significant dependencies
or overheads to the otherwise simple and efficient implementation. In
particular, the transactions provided by **kcas** do not directly provide
blocking or the ability to wait for changes to shared memory locations before
retrying a transaction. The way
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-commit)
works is that it simply retries the transaction in case it failed. To avoid
contention, a
[`backoff`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Backoff/index.html)
mechanism is used, but otherwise
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-commit)
will essentially perform a
[busy-wait](https://en.wikipedia.org/wiki/Busy_waiting), which should usually be
avoided.

### Programming with explicit transaction log passing

The [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html)
module provides an API that allows transactions to be implemented by explicitly
passing a mutable transaction log, which allows convenient use of all the
ordinary sequential control flow structures of OCaml.

> At the moment it is unclear whether both of the
> [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html) and
> the [`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html)
> APIs will be supported in the future. They provide roughly the same expressive
> power as witnessed by the conversions
> [`of_tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-of_tx)
> and
> [`to_tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-to_tx).
> Feedback on this question is welcome!

#### A transactional lock-free leftist heap

Let's implement something a bit more complicated,
[a leftist heap](https://en.wikipedia.org/wiki/Leftist_tree), which is a kind of
priority queue.

> The implementation here is adapted from the book _Data Structures and
> Algorithm Analysis in C (2nd ed.)_ by Mark Allen Weiss.

First we define a data type to represent the spine of a leftist heap:

```ocaml
# type 'v leftist =
    [ `Null
    | `Node of 'v leftist Loc.t
             * int Loc.t
             * 'v
             * 'v leftist Loc.t ]
type 'v leftist =
    [ `Node of 'v leftist Loc.t * int Loc.t * 'v * 'v leftist Loc.t | `Null ]
```

To create a leftist heap we
[`make`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-make)
a location with an empty spine:

```ocaml
# let leftist () : _ leftist Loc.t = Loc.make `Null
val leftist : unit -> 'a leftist Loc.t = <fun>
```

We then define an auxiliary function `npl_of` to get the null path length of a
leftist heap:

```ocaml
# let npl_of ~xt : _ leftist -> int = function
    | `Null -> 0
    | `Node (_, npl, _, _) -> Xt.get ~xt npl
val npl_of : xt:'a Xt.t -> 'b leftist -> int = <fun>
```

Notice the `~xt` parameter. It refers to the transaction log being passed
explicitly. Above we pass it to
[`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-get)
to record an operation in the log.

The core operation of leftist heaps is that of merging two leftist heaps:

```ocaml
# let rec merge ~xt ~lt h1 h2 =
    match h1, h2 with
    | `Null, h2 -> h2
    | h1, `Null -> h1
    | (`Node (_, _, v1, _) as h1),
      (`Node (_, _, v2, _) as h2) ->
      let (`Node (h1l, npl, _, h1r) as h1), h2 =
        if lt v1 v2 then h1, h2 else h2, h1 in
      let l = Xt.get ~xt h1l in
      if l == `Null then
        Xt.set ~xt h1l h2
      else begin
        let r = merge ~xt ~lt (Xt.get ~xt h1r) h2 in
        match npl_of ~xt l, npl_of ~xt r with
        | l_npl, r_npl when l_npl < r_npl ->
          Xt.set ~xt h1l r;
          Xt.set ~xt h1r l;
          Xt.set ~xt npl (l_npl + 1)
        | _, r_npl ->
          Xt.set ~xt h1r r;
          Xt.set ~xt npl (r_npl + 1)
      end;
      h1
val merge :
  xt:'a Xt.t ->
  lt:('b -> 'b -> bool) -> 'b leftist -> 'b leftist -> 'b leftist = <fun>
```

Again, `merge` passes the `~xt` parameter explicitly to the
[`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-get)
and
[`set`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-set)
operations to record them in the log.

The `merge` operation can be used to implement both insertion to

```ocaml
# let insert ~xt ~lt h x =
    let h1 = `Node (
        Loc.make `Null,
        Loc.make 1,
        x,
        Loc.make `Null
      ) in
    Xt.set ~xt h (merge ~xt ~lt h1 (Xt.get ~xt h))
val insert :
  xt:'a Xt.t -> lt:('b -> 'b -> bool) -> 'b leftist Loc.t -> 'b -> unit =
  <fun>
```

and deletion from

```ocaml
# let delete_min_opt ~xt ~lt h =
    match Xt.get ~xt h with
    | `Null -> None
    | `Node (h1, _, x, h2) ->
        Xt.set ~xt h
          (merge ~xt ~lt (Xt.get ~xt h1) (Xt.get ~xt h2));
        Some x
val delete_min_opt :
  xt:'a Xt.t -> lt:('b -> 'b -> bool) -> 'b leftist Loc.t -> 'b option =
  <fun>
```

a leftist heap.

Let's then first pick an ordering

```ocaml
# let lt = (>)
val lt : 'a -> 'a -> bool = <fun>
```

and create a leftist heap:

```ocaml
# let a_heap : int leftist Loc.t = leftist ()
val a_heap : int leftist Loc.t = <abstr>
```

To populate the heap we need to define a transaction passing function and pass
it to
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit):

```ocaml
# Xt.commit { tx = fun ~xt ->
    List.iter (insert ~xt ~lt a_heap) [3; 1; 4; 1; 5] }
- : unit = ()
```

Notice that we could simply use `List.iter` from the Stdlib to iterate over a
list of elements.

> The
> [`{ tx = ... }`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#type-tx)
> wrapper is used to ensure that the transaction function is polymorphic with
> respect to the log. This way the type system makes it difficult to
> accidentally leak the log as described in the paper
> [Lazy Functional State Threads](https://dl.acm.org/doi/10.1145/178243.178246).

Let's then define a transaction passing function to remove all elements from a
heap

```ocaml
# let remove_all ~xt ~lt h =
    let xs = ref [] in
    while match delete_min_opt ~xt ~lt h with
          | None -> false
          | Some x -> xs := x :: !xs; true do
      ()
    done;
    List.rev !xs
val remove_all :
  xt:'a Xt.t -> lt:('b -> 'b -> bool) -> 'b leftist Loc.t -> 'b list = <fun>
```

and use it

```ocaml
# Xt.commit { tx = remove_all ~lt a_heap }
- : int list = [5; 4; 3; 1; 1]
```

on the heap we populated earlier.

Notice how we were able to use a `while` loop, rather than recursion, in
`remove_all`.

> This leftist tree implementation is unlikely to be the best performing
> lock-free heap implementation, but it was pretty straightforward to implement
> using k-CAS based on a textbook imperative implementation.

## Development

### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### To make a new release

1. Update [CHANGES.md](CHANGES.md).
2. Run `dune-release tag VERSION` to create a tag for the new `VERSION`.
3. Run `dune-release` to publish the new `VERSION`.
4. Run `./update-gh-pages-for-tag VERSION` to update the online documentation.
