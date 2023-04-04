[API reference](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/index.html)
&middot; <sub><sup>(The API was redesigned in version 0.2.0. See
[API reference for version 0.1.8](https://ocaml-multicore.github.io/kcas/0.1.8/kcas/Kcas/index.html).)</sup></sub>

# **kcas** &mdash; Multi-word compare-and-set library

**kcas** provides an implementation of atomic
[lock-free](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom)
multi-word [compare-and-set](https://en.wikipedia.org/wiki/Compare-and-swap)
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
    - [A composable Michael-Scott style queue](#a-composable-michael-scott-style-queue)
- [Designing lock-free algorithms with k-CAS](#designing-lock-free-algorithms-with-k-cas)
  - [Minimize accesses](#minimize-accesses)
    - [Prefer compound accesses](#prefer-compound-accesses)
    - [Log updates optimistically and abort](#log-updates-optimistically-and-abort)
  - [Postcompute](#postcompute)
  - [Race to cooperate](#race-to-cooperate)
    - [Understanding transactions](#understanding-transactions)
    - [A three-stack lock-free queue](#a-three-stack-lock-free-queue)
    - [A rehashable lock-free hash table](#a-rehashable-lock-free-hash-table)
  - [Beware of torn reads](#beware-of-torn-reads)
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

#### A composable Michael-Scott style queue

One of the most famous lock-free algorithms is
[the Michael-Scott queue](https://www.cs.rochester.edu/~scott/papers/1996_PODC_queues.pdf).
Perhaps its characteristic feature is that the tail pointer of the queue is
allowed to momentarily fall behind and that operations on the queue perform
cooperative CASes to update the tail. The tail pointer can be seen as an
optimization &mdash; whether it points to the true tail or not does not change
the logical state of the queue. Let's implement a composable queue that allows
the tail to momentarily lag behind.

First we define a type for nodes:

```ocaml
# type 'a node = Nil | Node of 'a * 'a node Loc.t
type 'a node = Nil | Node of 'a * 'a node Loc.t
```

A queue is then a pair of pointers to the head and tail of a queue:

```ocaml
# type 'a queue = {
    head : 'a node Loc.t Loc.t;
    tail : 'a node Loc.t Atomic.t;
  }
type 'a queue = {
  head : 'a node Loc.t Loc.t;
  tail : 'a node Loc.t Atomic.t;
}
```

Note that we used an `Atomic.t` for the tail. We do not need to operate on the
tail transactionally.

To create a queue we allocate a shared memory location for the pointer to the
first node to be enqueued and make both the head and tail point to the location:

```ocaml
# let queue () =
    let next = Loc.make Nil in
    { head = Loc.make next; tail = Atomic.make next }
val queue : unit -> 'a queue = <fun>
```

To dequeue a node, only the head of the queue is examined. If the location
pointed to by the head points to a node we update the head to point to the
location pointing to the next node:

```ocaml
# let try_dequeue ~xt { head; _ } =
    let old_head = Xt.get ~xt head in
    match Xt.get ~xt old_head with
    | Nil -> None
    | Node (value, next) ->
      Xt.set ~xt head next;
      Some value
val try_dequeue : xt:'a Xt.t -> 'b queue -> 'b option = <fun>
```

To enqueue a value into the queue, only the tail of the queue needs to be
examined. We allocate a new location for the new tail and a node. We then need
to find the true tail of the queue and update it to point to the new node. The
reason we need to find the true tail is that we explicitly allow the tail to
momentarily fall behind. We then add a post commit action to the transaction to
update the tail after the transaction has been successfully committed:

```ocaml
# let enqueue ~xt { tail; _ } value =
    let new_tail = Loc.make Nil in
    let new_node = Node (value, new_tail) in
    let rec find_and_set_tail old_tail =
      match Xt.compare_and_swap ~xt old_tail Nil new_node with
      | Nil -> ()
      | Node (_, old_tail) -> find_and_set_tail old_tail
    in
    find_and_set_tail (Atomic.get tail);
    let rec fix_tail () =
      let old_tail = Atomic.get tail in
      if
        Loc.get new_tail == Nil
        && not (Atomic.compare_and_set tail old_tail new_tail)
      then fix_tail ()
    in
    Xt.post_commit ~xt fix_tail
val enqueue : xt:'a Xt.t -> 'b queue -> 'b -> unit = <fun>
```

The post commit action, registered using
[`post_commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-post_commit),
checks that the tail is still the true tail and then attempts to update the
tail. The order of accesses is very subtle as always with non-transactional
atomic operations. Can you see why it works? Although we allow the tail to
momentarily fall behind, it is important that we do not let the tail fall behind
indefinitely, because then we would risk leaking memory &mdash; nodes that have
been dequeued from the queue would still be pointed to by the tail.

Using the Michael-Scott style queue is as easy as any other transactional queue:

```ocaml
# let a_queue : int queue = queue ()
val a_queue : int queue = {head = <abstr>; tail = <abstr>}
# Xt.commit { tx = enqueue a_queue 19 }
- : unit = ()
# Xt.commit { tx = try_dequeue a_queue }
- : int option = Some 19
# Xt.commit { tx = try_dequeue a_queue }
- : int option = None
```

The queue implementation in this section is an example of using **kcas** to
implement a fine-grained lock-free algorithm. Instead of recording all shared
memory accesses and performing them atomically all at once, the implementation
updates the tail outside of the transaction. This can potentially improve
performance and scalability.

This sort of algorithm design requires careful reasoning. Consider the dequeue
operation. Instead of recording the `Xt.get ~xt old_head` operation in the
transaction log, one could propose to bypass the log as `Loc.get old_head`. That
may seem like a valid optimization, because logging the update of the head in
the transaction is sufficient to ensure that each transaction dequeues a unique
node. Unfortunately that would change the semantics of the operation.

Suppose, for example, that you have two queues, _A_ and _B_, and you must
maintain the invariant that at least one of the queues is empty. One domain
tries to dequeue from _A_ and, if _A_ was empty, enqueue to _B_. Another domain
does the opposite, dequeue from _B_ and enqueue to _A_ (when _B_ was empty).
When such operations are performed in isolation, the invariant would be
maintained. However, if the access of `old_head` is not recorded in the log, it
is possible to end up with both _A_ and _B_ non-empty. This kind of
[race condition](https://en.wikipedia.org/wiki/Race_condition) is common enough
that it has been given a name: _write skew_. As an exercise, write out the
sequence of atomic accesses that leads to that result.

## Designing lock-free algorithms with k-CAS

The key benefit of k-CAS, or k-CAS-n-CMP, and transactions in particular, is
that it allows developing lock-free algorithms compositionally. In the following
sections we discuss a number of basic tips and approaches for making best use of
k-CAS.

> The examples in this section use the
> [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html)
> module, but essentially the same advice applies when using the
> [`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html)
> module.

### Minimize accesses

Accesses of shared memory locations inside transactions consult the transaction
log. While the log is optimized, it still adds overhead. For best performance it
can be advantageous to minimize the number of accesses.

#### Prefer compound accesses

For best performance it can be advantageous to use compound accesses such as
[`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-update),
[`exchange`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-exchange),
and
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-modify)
instead of
[`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-get)
and
[`set`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-set),
because the compound accesses only consult the transaction log once.

Consider the following example that swaps the values of the shared memory
locations `a` and `b`:

```ocaml
# Xt.commit { tx = fun ~xt ->
    let a_val = Xt.get ~xt a
    and b_val = Xt.get ~xt b in
    Xt.set ~xt a b_val;
    Xt.set ~xt b a_val }
- : unit = ()
```

The above performs four accesses. Using
[`exchange`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-exchange)
we can reduce that to three:

```ocaml
# Xt.commit { tx = fun ~xt ->
    let a_val = Xt.get ~xt a in
    let b_val = Xt.exchange ~xt b a_val in
    Xt.set ~xt a b_val }
- : unit = ()
```

The above will likely perform slightly better.

> **_Question_**: _How does one count the number of accesses to the transaction
> log?_
>
> It is simple. Basically all of the access operations perform only a single
> access to the log. For simplicity, the documentation is written as if
> [`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-get)
> and
> [`set`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-set)
> were primitive, but all operations are actually implemented in terms of a more
> general operation similar to
> [`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-update),
> that only performs a single access to the transaction log.

#### Log updates optimistically and abort

Transactional write accesses to shared memory locations are only attempted after
the transaction log construction finishes successfully. Therefore it is entirely
safe to optimistically log updates against shared memory locations, validate
them during the log construction, and abort the transaction in case validation
fails.

Consider the following function to transfer an amount from specified source
location to specified target location:

```ocaml
# let transfer amount ~source ~target =
    let tx ~xt =
      if amount <= Xt.get ~xt source then begin
        Xt.set ~xt source (Xt.get ~xt source - amount);
        Xt.set ~xt target (Xt.get ~xt target + amount)
      end
    in
    Xt.commit { tx }
val transfer : int -> source:int Loc.t -> target:int Loc.t -> unit = <fun>
```

The above first examine the source location and then updates both source and
target. In a successful case it makes a total of five accesses. Using compound
accesses and optimistic updates we can reduce that to just two accesses:

```ocaml
# let transfer amount ~source ~target =
    let tx ~xt =
      if Xt.fetch_and_add ~xt source (-amount) < amount then
        raise Not_found;
      Xt.fetch_and_add ~xt target amount |> ignore
    in
    try Xt.commit { tx } with Not_found -> ()
val transfer : int -> source:int Loc.t -> target:int Loc.t -> unit = <fun>
```

Note that we raise the Stdlib `Not_found` exception to abort the transaction. As
we can see

```ocaml
# Loc.get a, Loc.get b
- : int * int = (52, 52)
# transfer 100 ~source:a ~target:b
- : unit = ()
# Loc.get a, Loc.get b
- : int * int = (52, 52)
# transfer 10 ~source:a ~target:b
- : unit = ()
# Loc.get a, Loc.get b
- : int * int = (42, 62)
```

the updates are only done in case of success.

### Postcompute

The more time a transaction takes, the more likely it is to suffer from
interference or even starvation. For best performance it is important to keep
transactions as short as possible. In particular, when possible, perform
expensive computations after the transactions.

Consider the following example of computing the size of a stack:

```ocaml
# let a_stack = Loc.make [2; 3]
val a_stack : int list Loc.t = <abstr>
# let n_elems =
    let tx ~xt =
      Xt.get ~xt a_stack
      |> List.length
    in
    Xt.commit { tx }
val n_elems : int = 2
```

The problem is that the computation of the list length is potentially expensive
and opens a potentially long time window for other domains to interfere.

In this case we can trivially move the list length computation outside of the
transaction:

```ocaml
# let n_elems =
    Xt.commit { tx = Xt.get a_stack }
    |> List.length
val n_elems : int = 2
```

As a more general approach, one could e.g. use closures to move compute after
transactions:

```ocaml
# let n_elems =
    let tx ~xt =
      let xs = Xt.get ~xt a_stack in
      fun () -> List.length xs
    in
    Xt.commit { tx } ()
val n_elems : int = 2
```

### Race to cooperate

Sometimes it is necessary to perform slower transactions that access many shared
memory locations or need to perform expensive computations during the
transaction. As mentioned previously, such transactions are more likely to
suffer from interference or even starvation as other transactions race to make
conflicting mutations to shared memory locations. To avoid such problems, it is
often possible to split the transaction into two:

1. A quick transaction that adversarially races against others.
2. A slower transaction that others will then cooperate to complete.

This lock-free algorithm design technique and the examples in the following
subsections are more advanced than the basic techniques described previously. To
understand and reason about these examples it is necessary to have a good
understanding of how transactions work.

#### Understanding transactions

We have previously casually talked about "transactions". Let's sharpen our
understanding of transactions.

In **kcas**, a _transaction_ is essentially a function that can be called to
prepare a specification of an operation or operations, in the form of a
_transaction log_, that can then be _attempted to be performed atomically_ by
the underlying k-CAS-n-CMP algorithm provided by **kcas**.

In other words, and simplifying a bit, when an explicit attempt is made to
perform a transaction, it basically proceeds in phases:

1. The first phase records a log of operations to access shared memory
   locations.

2. The second phase attempts to perform the operations atomically.

Either of the phases may fail. The first phase, which is under the control of
the transaction function, may raise an exception to abort the attempt. The
second phase fails when the accesses recorded in the transaction log are found
to be inconsistent with the contents of the shared memory locations. That
happens when the shared memory locations are mutated outside of the accesses
specified in the transaction log regardless of who made those mutations.

A transaction is not itself atomic and the construction of a transaction log, by
recording accesses of shared memory locations to the log, does not logically
mutate any shared memory locations.

When a transaction is (unconditionally) _committed_, rather than merely
_attempted_ (once), the commit mechanism keeps on retrying until an attempt
succeeds or the transaction function raises an exception (other than `Exit` or
[`Interference`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Mode/index.html#exception-Interference))
that the commit mechanism does not handle.

Each attempt or retry calls the transaction function again. This means that any
_side-effects_ within the transaction function are also performed again.

In previous sections we have used transactions as a coarse-grained mechanism to
encompass all shared memory accesses of the algorithm being implemented. This
makes it easy to reason about the effects of committing a transaction as the
accesses are then all performed as a single atomic operation. In the following
examples we will use our deeper understanding of transactions to implement more
fine-grained algorithms.

#### A three-stack lock-free queue

Recall the [two-stack queue](#a-transactional-lock-free-queue) discussed
earlier. The problem is that the `try_dequeue` operation `rev`erses the `back`
of the queue and that can be relatively expensive. One way to avoid that problem
is to introduce a third "middle" stack, or shared memory location, to the queue
and quickly move the back to the middle stack.

First we redefine the `queue` type to include a `middle` location:

```ocaml
# type 'a queue = {
    back : 'a list Loc.t;
    middle : 'a list Loc.t;
    front : 'a list Loc.t;
  }
type 'a queue = {
  back : 'a list Loc.t;
  middle : 'a list Loc.t;
  front : 'a list Loc.t;
}
```

And adjust the `queue` constructor function accordingly:

```ocaml
# let queue () =
    let back = Loc.make []
    and middle = Loc.make []
    and front = Loc.make [] in
    { back; middle; front }
val queue : unit -> 'a queue = <fun>
```

The `enqueue` operation remains essentially the same:

```ocaml
# let enqueue ~xt queue elem =
    Xt.modify ~xt queue.back @@ List.cons elem
val enqueue : xt:'a Xt.t -> 'b queue -> 'b -> unit = <fun>
```

For the quick transaction we introduce a helper function:

```ocaml
# let back_to_middle queue =
    let tx ~xt =
      match Xt.exchange ~xt queue.back [] with
      | [] -> raise Not_found
      | xs ->
        if Xt.exchange ~xt queue.middle xs != [] then
          raise Not_found
    in
    try Xt.commit { tx } with Not_found -> ()
val back_to_middle : 'a queue -> unit = <fun>
```

Note that the above uses
[`exchange`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-exchange)
to optimistically record shared memory accesses and then uses the `Not_found`
exception to abort the transaction in case the optimistic accesses turn out to
be unnecessary or incorrect.

The `dequeue` operation then runs the quick transaction to move elements from
the `back` to the `middle` before examining the `middle`:

```ocaml
# let dequeue ~xt queue =
    match Xt.update ~xt queue.front tl_safe with
    | x :: _ -> Some x
    | [] ->
      if not (Xt.is_in_log ~xt queue.middle ||
              Xt.is_in_log ~xt queue.back) then
        back_to_middle queue;
      match Xt.exchange ~xt queue.middle [] |> List.rev with
      | x :: xs ->
        Xt.set ~xt queue.front xs;
        Some x
      | [] ->
        match Xt.exchange ~xt queue.back [] |> List.rev with
        | x :: xs ->
          Xt.set ~xt queue.front xs;
          Some x
        | [] -> None
val dequeue : xt:'a Xt.t -> 'b queue -> 'b option = <fun>
```

There are a number of subtle implementation details above that deserve
attention.

First of all, notice that `dequeue` calls `back_to_middle queue` before
accessing `queue.middle` and `queue.back`. If the call `back_to_middle queue`
would be made after accessing `queue.middle` or `queue.back`, then those
accesses would be recorded in the transaction log `xt` and the log would be
inconsistent after `back_to_middle queue` mutates the locations. This would
cause the transaction attempt to fail and we want to avoid such doomed attempts.

Another subtle, but important, detail is that despite calling
`back_to_middle queue` to move `queue.back` to `queue.middle`, it would be
incorrect to assume that `queue.back` would be empty or that `queue.middle`
would be non-empty. That is because we must assume other domains may be
performing operations on the queue simultaneously. Another domain may have
pushed new elements to the `queue.back` or emptied `queue.middle`. Therefore we
meticulously examine both `queue.middle` and `queue.back`, if necessary. If we
don't do that, then it is possible that we incorrectly report the queue as being
empty.

Also, as should be clear, the side-effect performed by calling
`back_to_middle queue` is committed immediately every time it is called
regardless of the outcome of the transaction attempt. This is safe, because
`back_to_middle queue` does not logically change the state of the queue. It
merely performs a helping step, that is invisible to outside observers, towards
advancing the internal state of the queue. This is a common pattern in lock-free
algorithms.

As subtle as these kinds of lock-free algorithms are, this approach avoids the
potential starvation problems as now consumers do not attempt a slow transaction
to race against producers. Rather, the consumers perform quick adversarial races
against producers and then cooperatively race to complete the slow transaction.

> The three-stack queue presented in this section seems to perform reasonably
> well, should not suffer from most concurrency problems, and can be used
> compositionally.

#### A rehashable lock-free hash table

The previous example of adding a `middle` stack to the queue may seem like a
special case. Let's implement a simple lock-free hash table and, along the way,
examine a simple general way to replace a slow transaction with a quick
adversarial transaction and a slow cooperative transaction.

The difficulty with hash tables is rehashing. Let's ignore that for now and
implement a hash table without rehashing. For further simplicity, let's just use
separate chaining. Here is a type for such a basic hash table:

```ocaml
# type ('k, 'v) basic_hashtbl = {
    size: int Loc.t;
    data: ('k * 'v Loc.t) list Loc.t array Loc.t;
  }
type ('k, 'v) basic_hashtbl = {
  size : int Loc.t;
  data : ('k * 'v Loc.t) list Loc.t array Loc.t;
}
```

The basic hash table constructor just allocates all the locations:

```ocaml
# let basic_hashtbl () = {
    size = Loc.make 0;
    data = Loc.make (Array.init 4 (fun _ -> Loc.make []))
  }
val basic_hashtbl : unit -> ('a, 'b) basic_hashtbl = <fun>
```

Note that we (intentionally) used a very small capacity for the `data` table. In
a real implementation you'd probably want to have a bigger minimum capacity (and
might e.g. want to use a prime number).

Before tackling the basic operations, let's implement a helper function for
accessing the association list location corresponding to specified key:

```ocaml
# let access ~xt basic_hashtbl key =
    let data = Xt.get ~xt basic_hashtbl.data in
    let n = Array.length data in
    let i = Hashtbl.hash key mod n in
    data.(i)
val access :
  xt:'a Xt.t -> ('b, 'c) basic_hashtbl -> 'd -> ('b * 'c Loc.t) list Loc.t =
  <fun>
```

Now, to find an element, we access the association list and try to find the
key-value -pair:

```ocaml
# let find ~xt hashtbl key =
    let assoc_loc = access ~xt hashtbl key in
    Xt.get ~xt (List.assoc key (Xt.get ~xt assoc_loc))
val find : xt:'a Xt.t -> ('b, 'c) basic_hashtbl -> 'b -> 'c = <fun>
```

When replacing (or adding) the value corresponding to a key, we need to take
care to update the size when necessary:

```ocaml
# let replace ~xt hashtbl key value =
    let assoc_loc = access ~xt hashtbl key in
    let assoc = Xt.get ~xt assoc_loc in
    try
      let value_loc = List.assoc key assoc in
      Xt.set ~xt value_loc value
    with Not_found ->
      Xt.set ~xt assoc_loc ((key, Loc.make value) :: assoc);
      Xt.incr ~xt hashtbl.size
val replace : xt:'a Xt.t -> ('b, 'c) basic_hashtbl -> 'b -> 'c -> unit =
  <fun>
```

Removing an association also involves making sure that the size is updated
correctly:

```ocaml
# let remove ~xt hashtbl key =
    let assoc_loc = access ~xt hashtbl key in
    let rec loop ys = function
      | ((key', _) as y) :: xs ->
        if key <> key' then
          loop (y :: ys) xs
        else begin
          Xt.set ~xt assoc_loc (List.rev_append ys xs);
          Xt.decr ~xt hashtbl.size
        end
      | [] -> ()
    in
    loop [] (Xt.get ~xt assoc_loc)
val remove : xt:'a Xt.t -> ('b, 'c) basic_hashtbl -> 'b -> unit = <fun>
```

Now, the problem with the above is the lack of rehashing. As more associations
are added, performance deteriorates. We could implement a naive rehashing
operation:

```ocaml
let rehash ~xt hashtbl new_capacity =
  let new_data = Array.init new_capacity (fun _ -> Loc.make []) in
  Xt.exchange ~xt hashtbl.data new_data
  |> Array.iter (fun assoc_loc ->
     Xt.get ~xt assoc_loc
     |> List.iter (fun ((key, _) as bucket) ->
        let i = Hashtbl.hash key mod new_capacity in
        Xt.modify ~xt new_data.(i) (List.cons bucket)));
```

But that involves reading all the bucket locations. Any mutation that adds or
removes an association would cause such a rehash to fail.

To avoid taking on such adversarial races, we can use a level of indirection:

```ocaml
# type ('k, 'v) hashtbl = {
    pending: [`Nothing | `Rehash of int] Loc.t;
    basic: ('k, 'v) basic_hashtbl;
  }
type ('k, 'v) hashtbl = {
  pending : [ `Nothing | `Rehash of int ] Loc.t;
  basic : ('k, 'v) basic_hashtbl;
}
```

The idea is that a hash table is either considered to be normally accessible or
in the middle of being rehashed. It is easy to use this approach even when there
are many different slow operations.

Finding an element does not require mutating any locations, so we might just as
well allow those also during rehashes:

```ocaml
# let find ~xt hashtbl key = find ~xt hashtbl.basic key
val find : xt:'a Xt.t -> ('b, 'c) hashtbl -> 'b -> 'c = <fun>
```

Then we use a similar trick as with the three-stack queue. We use a quick
adversarial transaction to switch a hash table to the rehashing state in case a
rehash seems necessary:

```ocaml
# let prepare_rehash ~xt hashtbl delta =
    let tx ~xt =
      match Xt.get ~xt hashtbl.pending with
      | `Rehash _ -> ()
      | `Nothing ->
        let size = Int.max 1 (Xt.get ~xt hashtbl.basic.size + delta) in
        let capacity = Array.length (Xt.get ~xt hashtbl.basic.data) in
        if capacity < size * 4 then
          Xt.set ~xt hashtbl.pending (`Rehash (capacity * 2))
        else if size * 8 < capacity then
          Xt.set ~xt hashtbl.pending (`Rehash (capacity / 2))
        else
          raise Not_found
    in
    try
      if Xt.is_in_log ~xt hashtbl.pending then
        tx ~xt
      else
        Xt.commit { tx }
    with Not_found -> ()
val prepare_rehash : xt:'a Xt.t -> ('b, 'c) hashtbl -> int -> unit = <fun>
```

Note again that while the rehash logic allows some slack in the capacity, a real
implementation would likely use a bigger minimum capacity and perhaps avoid
using powers of two. Also, if we have already modified the hash table, which we
know by checking whether the `pending` location has been accessed, we must
continue within the same tramsaction.

Before we mutate a hash table, we will then call a helper to check whether we
need to rehash:

```ocaml
# let maybe_rehash ~xt hashtbl delta =
    prepare_rehash ~xt hashtbl delta;
    match Xt.get ~xt hashtbl.pending with
    | `Nothing -> ()
    | `Rehash new_capacity ->
      Xt.set ~xt hashtbl.pending `Nothing;
      rehash ~xt hashtbl.basic new_capacity
val maybe_rehash : xt:'a Xt.t -> ('b, 'c) hashtbl -> int -> unit = <fun>
```

Similarly to the previous example of
[a three-stack queue](#a-three-stack-lock-free-queue), a subtle, but important
detail is that the call to `prepare_rehash` is made before accessing
`hashtbl.pending`. This way the transaction log is not poisoned and there is
chance for the operation to succeed on the first attempt.

After switching to the rehashing state, all mutators will then cooperatively
race to perform the rehash.

We can now just implement the replace

```ocaml
# let replace ~xt hashtbl key value =
    maybe_rehash ~xt hashtbl (+1);
    replace ~xt hashtbl.basic key value
val replace : xt:'a Xt.t -> ('b, 'c) hashtbl -> 'b -> 'c -> unit = <fun>
```

and remove

```ocaml
# let remove ~xt hashtbl key =
    maybe_rehash ~xt hashtbl (-1);
    remove ~xt hashtbl.basic key
val remove : xt:'a Xt.t -> ('b, 'c) hashtbl -> 'b -> unit = <fun>
```

operations with rehashing.

After creating a constructor function

```ocaml
# let hashtbl () = {
    pending = Loc.make `Nothing;
    basic = basic_hashtbl ();
  }
val hashtbl : unit -> ('a, 'b) hashtbl = <fun>
```

for hash tables, we are ready to take it out for a spin:

```ocaml
# let a_hashtbl : (string, int) hashtbl = hashtbl ()
val a_hashtbl : (string, int) hashtbl =
  {pending = <abstr>; basic = {size = <abstr>; data = <abstr>}}
# let assoc = [
    ("Intro", 101);
    ("Answer", 42);
    ("OCaml", 5);
    ("Year", 2023)
  ]
val assoc : (string * int) list =
  [("Intro", 101); ("Answer", 42); ("OCaml", 5); ("Year", 2023)]
# assoc
  |> List.iter @@ fun (key, value) ->
     Xt.commit { tx = replace a_hashtbl key value }
- : unit = ()
# assoc
  |> List.iter @@ fun (key, _) ->
     Xt.commit { tx = remove a_hashtbl key }
- : unit = ()
```

What we have here is a lock-free hash table with rehashing that should not be
highly prone to starvation. In other respects this is a fairly naive hash table
implementation. You might want to think about various ways to improve upon it.

### Beware of torn reads

The algorithm underlying **kcas** ensures that it is not possible to read
uncommitted changes to shared memory locations and that an operation can only
complete successfully if all of the accesses taken together were atomic. These
are very strong guarantees and make it much easier to implement correct
concurrent algorithms.

Unfortunately, the transaction mechanism that **kcas** provides does not prevent
one specific concurrency anomaly. When reading multiple locations, it is
possible for a transaction to observe different locations at different times
even though it is not possible for the transaction to commit successfully unless
all the accesses together were atomic.

Let's examine this phenomena. To see the anomaly, we need to have two or more
locations. Let's just create two locations `a` and `b`:

```ocaml
# let a = Loc.make 0 and b = Loc.make 0
val a : int Loc.t = <abstr>
val b : int Loc.t = <abstr>
```

And create helper that spawns a domain that repeatedly increments `a` and
decrements `b` in a transaction:

```ocaml
# let with_updater fn =
    let stop = ref false in
    let domain = Domain.spawn @@ fun () ->
      while not !stop do
        let tx ~xt =
          Xt.incr ~xt a;
          Xt.decr ~xt b;
        in
        Xt.commit { tx }
      done in
    let finally () =
      stop := true;
      Domain.join domain in
    Fun.protect ~finally fn
val with_updater : (unit -> 'a) -> 'a = <fun>
```

The sum of the values of `a` and `b` must always be zero. We can verify this
using a transaction:

```ocaml
# with_updater @@ fun () ->
    for _ = 1 to 10_000 do
      let tx ~xt =
        0 = (Xt.get ~xt a + Xt.get ~xt b)
      in
      if not (Xt.commit { tx }) then
        failwith "torn read"
    done;
    "no torn reads"
- : string = "no torn reads"
```

Nice! So, it appears everything works as expected. A transaction can only commit
after having read a consistent, atomic, snapshot of all the shared memory
locations.

Unfortunately within a transaction attempt things are not as simple. Let's do an
experiment where we abort the transaction in case we observe that the values of
`a` and `b` are inconsistent:

```ocaml
# with_updater @@ fun () ->
    for _ = 1 to 10_000 do
      let tx ~xt =
        if 0 <> (Xt.get ~xt a + Xt.get ~xt b) then
          failwith "torn read"
      in
      Xt.commit { tx }
    done;
    "no torn reads"
Exception: Failure "torn read".
```

Oops! So, within a transaction we may actually observe different locations
having values from different committed transactions. This is something that
needs to be kept in mind when writing transactions.

## Development

### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### To make a new release

1. Update [CHANGES.md](CHANGES.md).
2. Run `dune-release tag VERSION` to create a tag for the new `VERSION`.
3. Run `dune-release` to publish the new `VERSION`.
4. Run `./update-gh-pages-for-tag VERSION` to update the online documentation.
