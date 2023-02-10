[API reference](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/index.html)
&middot; <sub><sup>(The API was redesigned in version 0.2.0. See
[API reference for version 0.1.8](https://ocaml-multicore.github.io/kcas/0.1.8/kcas/Kcas/index.html).)</sup></sub>

# **kcas** &mdash; Multi-word compare-and-swap library

**kcas** provides an implementation of atomic
[lock-free](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedomq)
multi-word [compare-and-swap](https://en.wikipedia.org/wiki/Compare-and-swap)
(MCAS), which is a powerful tool for designing concurrent algorithms.

Features and properties:

- **_Efficient_**: In the common uncontended case only **k + 1** single-word
  CASes are required per k-CAS.

- **_Lock-free_**: The underlying algorithm guarantees that at least one domain
  will be able to make progress.

- **_Disjoint-access parallel_**: Unrelated operations progress independently,
  without interference, even if they occur at the same time.

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
  locations, and

- [`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html),
  providing _composable transactions_ over shared memory locations.

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
[`Op`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Op/index.html)
and/or
[`Tx`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html) APIs
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
    get queue.front >>= function
    | element :: rest ->
      set queue.front rest >>.
      Some element
    | [] ->
      get_as List.rev queue.back >>= function
      | [] -> return None
      | element :: rest ->
        set queue.back [] >>
        set queue.front rest >>.
        Some element
  )
val try_dequeue : 'a queue -> 'a option Tx.t = <fun>
```

Above,
[`get_as`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-get_as)
and
[`>>`](<https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Tx/index.html#val-(%3E%3E)>)
are used as convenient shorthands. In fact, many of the combined primitives and
operators may also provide minor performance benefits.

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

## Development

### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### To make a new release

1. Update [CHANGES.md](CHANGES.md).
2. Run `dune-release tag VERSION` to create a tag for the new `VERSION`.
3. Run `dune-release` to publish the new `VERSION`.
4. Run `./update-gh-pages-for-tag VERSION` to update the online documentation.
