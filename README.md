[API reference](https://ocaml-multicore.github.io/kcas/doc/) &middot;
[Benchmarks](https://bench.ci.dev/ocaml-multicore/kcas/branch/main/benchmark/default)

<div align="center">

<a href="https://ocaml-multicore.github.io/kcas/">
  <img
    width="30%"
    alt="Kcas logo"
    src="https://raw.githubusercontent.com/ocaml-multicore/kcas/main/doc/kcas.svg">
</a>

# **Kcas** &mdash; Software Transactional Memory for OCaml

</div>

[**Kcas**](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/index.html)
provides a software transactional memory (STM) implementation based on an atomic
[lock-free](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Lock-freedom)
multi-word [compare-and-set](https://en.wikipedia.org/wiki/Compare-and-swap)
(MCAS) algorithm
[enhanced with read-only compare operations](doc/gkmz-with-read-only-cmp-ops.md)
and ability to block awaiting for changes.

[**Kcas_data**](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/index.html)
provides compositional lock-free data structures and primitives for
communication and synchronization implemented using **Kcas**.

Features and properties:

- **_Efficient_**: In the common uncontended case only **k + 1** single-word
  CASes are required per k-CAS and, as a special case, 1-CAS requires only a
  single single-word CAS.

- **_Lock-free_**: The underlying algorithm guarantees that at least one
  operation will be able to make progress.

- **_Disjoint-access parallel_**: Unrelated operations progress independently,
  without interference, even if they occur at the same time.

- **_Read-only compares_**: The algorithm supports
  [obstruction-free](https://en.wikipedia.org/wiki/Non-blocking_algorithm#Obstruction-freedom)
  read-only compare (CMP) operations that can be performed on overlapping
  locations in parallel without interference.

- **_Blocking await_**: The algorithm supports timeouts and awaiting for changes
  to any number of shared memory locations.

- **_Composable_**: Independently developed transactions can be composed with
  ease sequentially, conjunctively, conditionally, and disjunctively.

In other words,
[performance](https://bench.ci.dev/ocaml-multicore/kcas/branch/main/benchmark/default)
should be acceptable and scalable for many use cases, the non-blocking
properties should allow use in many contexts including those where locks are not
acceptable, and the features provided should support most practical needs.

**Kcas** is [published on **opam**](https://opam.ocaml.org/packages/kcas/) and
is distributed under the [ISC license](LICENSE.md).

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Focaml-multicore%2Fkcas%2Fmain&logo=ocaml&style=flat-square)](https://ci.ocamllabs.io/github/ocaml-multicore/kcas)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/ocaml-multicore/kcas?style=flat-square&color=09aa89)](https://github.com/ocaml-multicore/kcas/releases/latest)
[![docs](https://img.shields.io/badge/doc-online-blue.svg?style=flat-square)](https://ocaml-multicore.github.io/kcas/doc/)

## Contents

- [A quick tour](#a-quick-tour)
- [Introduction](#introduction)
  - [Creating and manipulating individual shared memory locations](#creating-and-manipulating-individual-shared-memory-locations)
  - [Programming with transactions](#programming-with-transactions)
    - [A transactional lock-free stack](#a-transactional-lock-free-stack)
    - [A transactional lock-free queue](#a-transactional-lock-free-queue)
    - [Composing transactions](#composing-transactions)
    - [Blocking transactions](#blocking-transactions)
    - [Timeouts](#timeouts)
    - [A transactional lock-free leftist heap](#a-transactional-lock-free-leftist-heap)
  - [Programming with transactional data structures](#programming-with-transactional-data-structures)
    - [The dining philosophers problem](#the-dining-philosophers-problem)
    - [A transactional LRU cache](#a-transactional-lru-cache)
- [Designing lock-free algorithms with k-CAS](#designing-lock-free-algorithms-with-k-cas)
  - [Understand performance](#understand-performance)
  - [Minimize accesses](#minimize-accesses)
    - [Prefer compound accesses](#prefer-compound-accesses)
    - [Log updates optimistically](#log-updates-optimistically)
  - [Postcompute](#postcompute)
  - [Post commit actions](#post-commit-actions)
    - [A composable Michael-Scott style queue](#a-composable-michael-scott-style-queue)
  - [Race to cooperate](#race-to-cooperate)
    - [Understanding transactions](#understanding-transactions)
    - [A three-stack lock-free queue](#a-three-stack-lock-free-queue)
    - [A rehashable lock-free hash table](#a-rehashable-lock-free-hash-table)
  - [Avoid false sharing](#avoid-false-sharing)
  - [Beware of torn reads](#beware-of-torn-reads)

## A quick tour

To use the library

<!--
```ocaml
# #thread
```
-->

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
# Loc.set a 10
- : unit = ()

# Loc.get a
- : int = 10

# Loc.compare_and_set b 0 52
- : bool = true

# Loc.get b
- : int = 52
```

Block waiting for changes to locations:

```ocaml
# let a_domain = Domain.spawn @@ fun () ->
    let x = Loc.get_as (fun x -> Retry.unless (x <> 0); x) x in
    Printf.sprintf "The answer is %d!" x
val a_domain : string Domain.t = <abstr>
```

Perform transactions over locations:

```ocaml
# let tx ~xt =
    let a = Xt.get ~xt a
    and b = Xt.get ~xt b in
    Xt.set ~xt x (b - a)
  in
  Xt.commit { tx }
- : unit = ()
```

And now we have it:

```ocaml
# Domain.join a_domain
- : string = "The answer is 42!"
```

## Introduction

The API of **Kcas** is divided into submodules. The main modules are

- [`Loc`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html),
  providing an abstraction of _shared memory locations_, and

- [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html),
  providing _explicit transaction log passing_ over shared memory locations.

The following sections discuss both of the above in turn.

### Creating and manipulating individual shared memory locations

The [`Loc`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html)
module is essentially compatible with the Stdlib
[`Atomic`](https://v2.ocaml.org/api/Atomic.html) module, except that a number of
functions take some optional arguments that one usually need not worry about.

In other words, an application that uses
[`Atomic`](https://v2.ocaml.org/api/Atomic.html), but then needs to perform
atomic operations over multiple atomic locations, could theoretically just
rebind `module Atomic = Loc` and then use the
[`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html) API
to perform operations over multiple locations. This should not be done
just-in-case, however, as, even though **Kcas** is efficient, it does naturally
have higher overhead than the Stdlib
[`Atomic`](https://v2.ocaml.org/api/Atomic.html).

### Programming with transactions

The [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html)
module provides an API that allows _transactions_ over shared memory locations
to be implemented as functions that explicitly pass a mutable transaction log,
as the labeled argument `~xt`, through the computation to record accesses of
shared memory locations. Once the transaction function returns, those accesses
can then be attempted to be performed atomically. The
[`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html) API
is intended to be suitable for both designing and implementing new lock-free
algorithms and as an application level programming interface for compositional
use of such algorithms.

#### A transactional lock-free stack

As our first example of using transactions, let's implement a lock-free stack. A
stack can be just a shared memory location that holds a list of elements:

```ocaml
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
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-modify)
the stack to cons the element onto the list:

```ocaml
# let push ~xt stack element =
    Xt.modify ~xt stack @@ List.cons element
val push : xt:'a Xt.t -> 'b list Loc.t -> 'b -> unit = <fun>
```

Notice the `~xt` parameter. It refers to the transaction log being passed
explicitly. Above we pass it to
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-modify)
to record an operation in the log rather than perform it immediately.

Popping an element from a stack is a little more complicated as we need to
handle the case of an empty stack. Let's go with a basic approach where we first
[`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-get)
the content of the stack, and
[`set`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-set)
it if necessary, and return an optional element.

```ocaml
# let try_pop ~xt stack =
    match Xt.get ~xt stack with
    | [] -> None
    | element :: rest ->
      Xt.set ~xt stack rest;
      Some element
val try_pop : xt:'a Xt.t -> 'b list Loc.t -> 'b option = <fun>
```

Again, `try_pop` passes the `~xt` parameter explicitly to the
[`get`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-get)
and
[`set`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-set)
operations to record them in the log rather than perform them immediately.

We could also implement `try_pop` more concisely with the help of a couple of
useful list manipulation helper functions

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

and
[`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-update):

```ocaml
# let try_pop ~xt stack =
    Xt.update ~xt stack tl_safe |> hd_opt
val try_pop : xt:'a Xt.t -> 'b list Loc.t -> 'b option = <fun>
```

If the stack already contained an empty list, `[]`, both of the above variations
of `try_pop` generate a read-only CMP operation in the
[`obstruction_free`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Mode/index.html#val-obstruction_free)
mode. This means that multiple domains may run `try_pop` on an empty stack in
parallel without interference. The variation using
[`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-update)
also makes only a single access to the underlying transaction log and is likely
to be the faster variation.

So, to use a stack, we first need to create it and then we may
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit)
transactions to `push` and `try_pop` elements:

```ocaml
# let a_stack : int stack = stack ()
val a_stack : int stack = <abstr>

# Xt.commit { tx = push a_stack 101 }
- : unit = ()

# Xt.commit { tx = try_pop a_stack }
- : int option = Some 101

# Xt.commit { tx = try_pop a_stack }
- : int option = None
```

The
[`{ tx = ... }`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#type-tx)
wrapper is used to ensure that the transaction function is polymorphic with
respect to the log. This way the type system makes it difficult to accidentally
leak the log as described in the paper
[Lazy Functional State Threads](https://dl.acm.org/doi/10.1145/178243.178246).

As an astute reader you may wonder why we wrote `push` and `try_pop` to take a
transaction log as a parameter and then separately called
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit)
rather than just call
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit)
inside the `push` and `try_pop` functions and avoid exposing the `~xt`
parameter. We'll get to that soon!

#### A transactional lock-free queue

Let's then implement a lock-free queue. To keep things simple we just use the
traditional two-stack queue data structure:

```ocaml
type 'a queue = {
  front: 'a list Loc.t;
  back: 'a list Loc.t;
}
```

To create a queue we
[`make`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-make)
the two locations:

```ocaml
# let queue () = {
    front = Loc.make [];
    back = Loc.make [];
  }
val queue : unit -> 'a queue = <fun>
```

To enqueue we just
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-modify)
the back of the queue and `cons` the element to the list:

```ocaml
# let enqueue ~xt queue element =
    Xt.modify ~xt queue.back @@ List.cons element
val enqueue : xt:'a Xt.t -> 'b queue -> 'b -> unit = <fun>
```

Dequeue is again more complicated. First we examine the front of the queue. If
there is an element, we update the front and return the element. If the front is
empty, we examine the back of the queue in `rev`erse. If there is an element we
clear the back, move the rest of the elements to the front, and return the
element. Otherwise we return `None` as the queue was empty.

```ocaml
# let try_dequeue ~xt queue =
    match Xt.update ~xt queue.front tl_safe with
    | element :: _ -> Some element
    | [] ->
      match Xt.exchange ~xt queue.back [] |> List.rev with
      | [] -> None
      | element :: rest ->
        Xt.set ~xt queue.front rest;
        Some element
val try_dequeue : xt:'a Xt.t -> 'b queue -> 'b option = <fun>
```

Above,
[`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-update)
and
[`exchange`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-exchange)
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
> [`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit)
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
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit)
transactions to `enqueue` and `try_dequeue` elements:

```ocaml
# let a_queue : int queue = queue ()
val a_queue : int queue = {front = <abstr>; back = <abstr>}

# Xt.commit { tx = enqueue a_queue 76 }
- : unit = ()

# Xt.commit { tx = try_dequeue a_queue }
- : int option = Some 76

# Xt.commit { tx = try_dequeue a_queue }
- : int option = None
```

> **_Beware_**: Using two stacks for a queue is easy to implement and performs
> ok in many cases. Unfortunately it has one major weakness. The problem is that
> it may take a relatively long time to reverse the back of a queue. This can
> cause
> [starvation](<https://en.wikipedia.org/wiki/Starvation_(computer_science)>) as
> producers may then be able to always complete their transactions before
> consumers and the back of the queue might grow without bound. _Can you see a
> way to avoid this problem?_

#### Composing transactions

The main feature of the
[`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html) API
is that transactions are composable. In fact, we already wrote transactions that
recorded multiple primitive shared memory accesses to the explicitly passed
transaction log. Nothing prevents us from writing transactions calling other
non-primitive transactions.

For example, one can write a transaction to push multiple elements to a
transactional stack atomically:

```ocaml
# let tx ~xt =
    push ~xt a_stack 3;
    push ~xt a_stack 1;
    push ~xt a_stack 4
  in
  Xt.commit { tx }
- : unit = ()
```

Or transfer elements between different transactional data structures:

```ocaml
# let tx ~xt =
    match try_pop ~xt a_stack with
    | Some element ->
      enqueue ~xt a_queue element
    | None ->
      ()
  in
  Xt.commit { tx }
- : unit = ()
```

The ability to compose transactions allows algorithms and data-structures to be
used for a wider variety of purposes.

#### Blocking transactions

All of the previous operations we have implemented on stacks and queues have
been non-blocking. What if we'd like to wait for an element to appear in a
stack? One could write a loop that keeps on trying to pop an element

```ocaml
# let rec busy_waiting_pop stack =
    match Xt.commit { tx = try_pop stack } with
    | None -> busy_waiting_pop stack
    | Some elem -> elem
val busy_waiting_pop : 'a list Loc.t -> 'a = <fun>
```

but this sort of [busy-wait](https://en.wikipedia.org/wiki/Busy_waiting) is
usually a _bad idea_ and should be avoided. It is usually better to block in
such a way that the underlying domain can potentially perform other work.

To support blocking **Kcas** provides a
[`later`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Retry/index.html#val-later)
operation that amounts to raising a
[`Later`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Retry/index.html#exception-Later)
exception signaling that the operation, whether a single location operation or a
multi location transaction, should be retried only after the shared memory
locations examined by the operation have been modified outside of the
transaction.

Using
[`later`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Retry/index.html#val-later)
we can easily write blocking pop

```ocaml
# let pop ~xt stack =
    match try_pop ~xt stack with
    | None -> Retry.later ()
    | Some elem -> elem
val pop : xt:'a Xt.t -> 'b list Loc.t -> 'b = <fun>
```

and dequeue

```ocaml
# let dequeue ~xt queue =
    match try_dequeue ~xt queue with
    | None -> Retry.later ()
    | Some elem -> elem
val dequeue : xt:'a Xt.t -> 'b queue -> 'b = <fun>
```

operations.

To test them out, let's create a fresh stack and a queue

```ocaml
# let a_stack : int stack = stack ()
val a_stack : int stack = <abstr>

# let a_queue : int queue = queue ()
val a_queue : int queue = {front = <abstr>; back = <abstr>}
```

and then spawn a domain that tries to atomically both pop and dequeue:

```ocaml
# let a_domain = Domain.spawn @@ fun () ->
    let tx ~xt = (pop ~xt a_stack, dequeue ~xt a_queue) in
    let (popped, dequeued) = Xt.commit { tx } in
    Printf.sprintf "I popped %d and dequeued %d!"
      popped dequeued
val a_domain : string Domain.t = <abstr>
```

The domain is now blocked waiting for changes to the stack and the queue. As
long as we don't populate both at the same time

```ocaml
# Xt.commit { tx = push a_stack 2 };
  let x = Xt.commit { tx = pop a_stack } in
  Xt.commit { tx = enqueue a_queue x }
- : unit = ()
```

the transaction keeps on being blocked. But if both become populated at the same
time

```ocaml
# Xt.commit { tx = push a_stack 4 }
- : unit = ()

# Domain.join a_domain
- : string = "I popped 4 and dequeued 2!"
```

the transaction can finish.

The retry mechanism essentially allows a transaction to wait for an arbitrary
condition and can function as a fairly expressive communication and
synchronization mechanism.

#### Timeouts

> If you block, will they come?

That is a good question. Blocking indefinitely is often not acceptable.

A blocked transaction can be waken up by a write to any shared memory location
that was accessed by the transaction. This means that, given a suitable timeout
mechanism, one could e.g. setup a timeout that writes to a boolean shared memory
location that is accessed by a blocking transaction:

```ocaml
# let pop_or_raise_if ~xt timeout stack =
    (* Check if timeout has expired: *)
    if Xt.get ~xt timeout then raise Exit;
    pop stack
val pop_or_raise_if :
  xt:'a Xt.t -> bool Loc.t -> 'b list Loc.t -> xt:'c Xt.t -> 'b = <fun>
```

This works, but creating, checking, and canceling timeouts properly can be a lot
of work. Therefore **Kcas** also directly supports an optional `timeoutf`
argument for potentially blocking operations. For example, to perform a blocking
pop with a timeout, one can simply explicitly pass the desired timeout in
seconds:

```ocaml
# let an_empty_stack = stack () in
  Xt.commit ~timeoutf:0.1 { tx = pop an_empty_stack }
Exception: Failure "Domain_local_timeout.set_timeoutf not implemented".
```

Oops! What happened above is that the
[_domain local timeout_](https://github.com/ocaml-multicore/domain-local-timeout)
mechanism used by **Kcas** was not implemented on the current domain. The idea
is that, in the future, concurrent schedulers provide the mechanism out of the
box, but there is also a default implementation using the Stdlib `Thread` and
`Unix` modules that works on most platforms. However, to avoid direct
dependencies to `Thread` and `Unix`, we need to explicitly tell the library that
it can use those modules:

```ocaml
# Domain_local_timeout.set_system (module Thread) (module Unix)
- : unit = ()
```

This initialization, if needed, should be done by application code rather than
by libraries.

If we now retry the previous example we will get a
[`Timeout`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Timeout/index.html#exception-Timeout)
exception as expected:

```ocaml
# let an_empty_stack = stack () in
  Xt.commit ~timeoutf:0.1 { tx = pop an_empty_stack }
Exception: Kcas.Timeout.Timeout.
```

Besides
[`commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-commit),
potentially blocking single location operations such as
[`get_as`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-get_as),
[`update`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-update),
and
[`modify`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-modify)
support the optional `timeoutf` argument.

#### A transactional lock-free leftist heap

Let's implement something a bit more complicated,
[a leftist heap](https://en.wikipedia.org/wiki/Leftist_tree), which is a kind of
priority queue.

> The implementation here is adapted from the book _Data Structures and
> Algorithm Analysis in C (2nd ed.)_ by Mark Allen Weiss.

First we define a data type to represent the spine of a leftist heap:

```ocaml
type 'v leftist =
  [ `Null
  | `Node of 'v leftist Loc.t
           * int Loc.t
           * 'v
           * 'v leftist Loc.t ]
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
# let tx ~xt =
    List.iter (insert ~xt ~lt a_heap) [3; 1; 4; 1; 5]
  in
  Xt.commit { tx }
- : unit = ()
```

Notice that we could simply use `List.iter` from the Stdlib to iterate over a
list of elements.

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

### Programming with transactional data structures

When was the last time you implemented a non-trivial data structure or algorithm
from scratch? For most professionals the answer might be along the lines of
_"when I took my data structures course at the university"_ or _"when I
interviewed for the software engineering position at Big Co"_.

**Kcas** aims to be usable both

- for experts implementing correct and performant lock-free data structures, and
- for everyone gluing together programs using such data structures.

Many of the examples in this introduction are data structures of some sort.
However, implementing basic data structures from scratch is not something
everyone should be doing every time they are writing concurrent programs.
Instead programmers should be able to reuse carefully constructed data
structures.

One source of ready-made data structures is
[**Kcas_data**](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/index.html).
Let's explore how we can leverage those data structures. Of course, first we
need to `#require` the package and we'll also open it for convenience:

```ocaml
# #require "kcas_data"
# open Kcas_data
```

#### The dining philosophers problem

The
[dining philosophers problem](https://en.wikipedia.org/wiki/Dining_philosophers_problem)
is a well known classic synchronization problem. It is easy to solve with
**Kcas**. If you are unfamiliar with the problem, please take a moment to read
the description of the problem.

A handy concurrent data structure for solving the dining philosophers problem is
the
[`Mvar`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Mvar/index.html)
or synchronizing variable. A `'a Mvar.t` is basically like a `'a option Loc.t`
with blocking semantics for both
[`take`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Mvar/index.html#val-take)
and
[`put`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Mvar/index.html#val-put).
For the dining philosophers problem, we can use `Mvar`s to store the forks.

The problem statement doesn't actually say when to stop. The gist of the
problem, of course, is that no philosopher should starve. So, we'll make it so
that we keep a record of how many times each philosopher has eaten. We'll then
end the experiment as soon as each philosopher has eaten some minimum number of
times. Programming a philosopher is now straightforward:

```ocaml
# let philosopher ~fork_lhs ~fork_rhs ~eaten ~continue =
    let eat () =
      let take_forks ~xt =
        ( Mvar.Xt.take ~xt fork_lhs,
          Mvar.Xt.take ~xt fork_rhs )
      in
      let (lhs, rhs) = Xt.commit { tx = take_forks } in

      Loc.incr eaten;

      let drop_forks () =
        Mvar.put fork_lhs lhs;
        Mvar.put fork_rhs rhs
      in
      drop_forks ()
    in

    while continue () do
      eat ()
    done
val philosopher :
  fork_lhs:'a Mvar.t ->
  fork_rhs:'b Mvar.t -> eaten:int Loc.t -> continue:(unit -> bool) -> unit =
  <fun>
```

The dining philosophers main routine then creates the data structures and spawns
the philosophers:

```ocaml
# let dinining_philosophers ~philosophers ~min_rounds =
    assert (3 <= philosophers && 0 <= min_rounds);
    let eaten = Loc.make_array philosophers 0 in
    let continue () =
      eaten
      |> Array.exists @@ fun eaten ->
         Loc.get eaten < min_rounds
    in
    let forks =
      Array.init philosophers @@ fun i ->
      Mvar.create (Some i)
    in
    Array.iter Domain.join @@ Array.init philosophers @@ fun i ->
      Domain.spawn @@ fun () ->
      let fork_lhs = forks.(i)
      and fork_rhs = forks.((i + 1) mod philosophers)
      and eaten = eaten.(i) in
      philosopher ~fork_lhs ~fork_rhs ~eaten ~continue
val dinining_philosophers : philosophers:int -> min_rounds:int -> unit =
  <fun>
```

We can now run our solution and confirm that it terminates after each
philosopher has eaten at least a given number of times:

```ocaml
# dinining_philosophers ~philosophers:5 ~min_rounds:1_000
- : unit = ()
```

What makes dining philosophers so easy to solve with transactions is that we can
simply compose two `take` operations to take both forks.

#### A transactional LRU cache

A LRU or least-recently-used cache is essentially a bounded association table.
When the capacity of the cache is exceeded, some association is dropped. The LRU
or least-recently-used policy is to drop the association that was accessed least
recently.

A simple way to implement a LRU cache is to use a hash table to store the
associations and a doubly-linked list to keep track of the order in which
associations have been accessed. Whenever an association is accessed, the
corresponding linked list node is added or moved to one end of the list. When
the cache overflows, the association whose node is at the other end of the list
is removed.

**Kcas_data** conveniently provides a
[`Hashtbl`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Hashtbl/index.html)
module providing a hash table implementation that mimics the Stdlib
[`Hashtbl`](https://v2.ocaml.org/api/Hashtbl.html) module and a
[`Dllist`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Dllist/index.html)
providing a doubly-linked list implementation. We'll also keep track of the
space in the cache using a separate shared memory location so that it is
possible to change the capacity of the cache dynamically:

```ocaml
type ('k, 'v) cache = {
  space: int Loc.t;
  table: ('k, 'k Dllist.node * 'v) Hashtbl.t;
  order: 'k Dllist.t;
}
```

To create a cache we just create the data structures:

```ocaml
# let cache ?hashed_type capacity = {
    space = Loc.make capacity;
    table = Hashtbl.create ?hashed_type ();
    order = Dllist.create ();
  }
val cache : ?hashed_type:'a Hashtbl.hashed_type -> int -> ('a, 'b) cache =
  <fun>
```

Note that above we just passed the optional `hashed_type` argument to the hash
table constructor. The hash table
[`create`](https://ocaml-multicore.github.io/kcas/doc/kcas_data/Kcas_data/Hashtbl/index.html#val-create)
function takes some more optional arguments some of which might make sense to
pass through.

To access an association in the cache we provide a `get_opt` operation

```ocaml
# let get_opt ~xt {table; order; _} key =
    Hashtbl.Xt.find_opt ~xt table key
    |> Option.map @@ fun (node, value) ->
       Dllist.Xt.move_l ~xt node order;
       value
val get_opt : xt:'a Xt.t -> ('b, 'c) cache -> 'b -> 'c option = <fun>
```

that, as explained previously, moves the node corresponding to the accessed
association to the left end of the list.

To introduce associations we provide the `set_blocking` operation

```ocaml
# let set_blocking ~xt {table; order; space; _} key value =
    let node =
      match Hashtbl.Xt.find_opt ~xt table key with
      | None ->
        if 0 = Xt.update ~xt space (fun n -> Int.max 0 (n-1)) then
          Dllist.Xt.take_blocking_r ~xt order
          |> Hashtbl.Xt.remove ~xt table;
        Dllist.Xt.add_l ~xt key order
      | Some (node, _) ->
        Dllist.Xt.move_l ~xt node order;
        node
    in
    Hashtbl.Xt.replace ~xt table key (node, value)
val set_blocking : xt:'a Xt.t -> ('b, 'c) cache -> 'b -> 'c -> unit = <fun>
```

that, like `get_opt`, either moves or adds the node of the accessed association
to the left end of the list. In case a new association is added, the space is
decremented. If there was no space, an association is first removed, which will
block in case capacity is 0. As described previously, the association to remove
is determined by removing the rightmost element from the list.

We can then test that the cache works as expected:

```ocaml
# let a_cache : (int, string) cache = cache 2
val a_cache : (int, string) cache =
  {space = <abstr>; table = <abstr>; order = <abstr>}

# Xt.commit { tx = set_blocking a_cache 101 "basics" }
- : unit = ()

# Xt.commit { tx = set_blocking a_cache 42 "answer" }
- : unit = ()

# Xt.commit { tx = get_opt a_cache 101 }
- : string option = Some "basics"

# Xt.commit { tx = set_blocking a_cache 2023 "year" }
- : unit = ()

# Xt.commit { tx = get_opt a_cache 42 }
- : string option = None
```

As an exercise, implement an operation to `remove` associations from a cache and
an operation to change the capacity of the cache.

## Designing lock-free algorithms with k-CAS

The key benefit of k-CAS, or k-CAS-n-CMP, and transactions in particular, is
that it allows developing lock-free algorithms compositionally. In the following
sections we discuss a number of basic tips and approaches for making best use of
k-CAS.

### Understand performance

It is possible to convert imperative sequential data structures to lock-free
data structures [almost](#beware-of-torn-reads) just by using
[shared memory locations](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/)
and wrapping everything inside
[transactions](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/), but
doing so will likely not lead to good performance.

On the other hand, if you have a non-blocking data structure implemented using
plain `Atomic`s, then simply replacing `Atomic` with
[`Loc`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/) you should
get a data structure that works the same and will take somewhat more memory and
operates somewhat more slowly. However, adding transactional operations simply
by wrapping all accesses of a non-blocking data structure implementation will
likely not lead to well performing transactional operations.

[Shared memory locations](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/)
take more memory than ordinary mutable fields or mutable references and mutating
operations on shared memory locations allocate. The
[transaction mechanism](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/)
also allocates and adds lookup overhead to accesses. Updating multiple locations
in a transaction is more expensive than updating individual locations
atomically. Contention can cause transactions to retry and perform poorly.

With that said, it is possible to create composable and reasonably well
performing data structures using **Kcas**. If a **Kcas** based data structure is
performing much worse than a similar lock-free or lock-based data structure,
then there is likely room to improve. Doing so will require good understanding
of and careful attention to algorithmic details, such as which accessed need to
be performed transactionally and which do not, operation of the transaction
mechanism, and performance of individual low level operations.

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
# let tx ~xt =
    let a_val = Xt.get ~xt a
    and b_val = Xt.get ~xt b in
    Xt.set ~xt a b_val;
    Xt.set ~xt b a_val
  in
  Xt.commit { tx }
- : unit = ()
```

The above performs four accesses. Using
[`exchange`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-exchange)
we can reduce that to three:

```ocaml
# let tx ~xt =
    let a_val = Xt.get ~xt a in
    let b_val = Xt.exchange ~xt b a_val in
    Xt.set ~xt a b_val
  in
  Xt.commit { tx }
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

#### Log updates optimistically

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
        raise Exit;
      Xt.fetch_and_add ~xt target amount |> ignore
    in
    try Xt.commit { tx } with Exit -> ()
val transfer : int -> source:int Loc.t -> target:int Loc.t -> unit = <fun>
```

Note that we raise the Stdlib `Exit` exception to abort the transaction. As we
can see

```ocaml
# Loc.get a, Loc.get b
- : int * int = (10, 52)

# transfer 100 ~source:a ~target:b
- : unit = ()

# Loc.get a, Loc.get b
- : int * int = (10, 52)

# transfer 10 ~source:a ~target:b
- : unit = ()

# Loc.get a, Loc.get b
- : int * int = (0, 62)
```

the updates are only done in case of success.

A problem with the `transfer` function above is that it is not a composable
transaction. The transaction mechanism provided by **Kcas** does not implicitly
perform rollbacks of changes made to locations, but it does offer low level
support for nested conditional transactions.

By explicitly calling
[`snapshot`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-snapshot)
and
[`rollback`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-rollback)
one can scope tentative changes and create a composable version of `transfer`:

```ocaml
# let transfer ~xt amount ~source ~target =
    let snap = Xt.snapshot ~xt in
    if Xt.fetch_and_add ~xt source (-amount) < amount then
      Retry.later (Xt.rollback ~xt snap);
    Xt.fetch_and_add ~xt target amount |> ignore
val transfer :
  xt:'a Xt.t -> int -> source:int Loc.t -> target:int Loc.t -> unit = <fun>
```

Given a bunch of locations

```ocaml
let a = Loc.make 10
and b = Loc.make 20
and c = Loc.make 30
and d = Loc.make 27
```

we can now attempt `transfer`s and perform the
[`first`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-first)
of them that succeeds:

```ocaml
# Xt.commit {
    tx = Xt.first [
      transfer 15 ~source:a ~target:d;
      transfer 15 ~source:b ~target:d;
      transfer 15 ~source:c ~target:d;
    ]
  }
- : unit = ()
```

A look at the locations

```ocaml
# List.map Loc.get [a; b; c; d]
- : int list = [10; 5; 30; 42]
```

confirms the expected result.

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

### Post commit actions

Closely related to moving compute outside of transactions, it is also sometimes
possible or necessary to perform some side-effects or actions, such as
non-transactional IO operations, only after a transaction has been committed
successfully. These cases are supported via the ability to register
[`post_commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-post_commit)
actions.

As a basic example, one might want to log a message when some transactional
operation is performed. Instead of directly logging the message

```ocaml
# let enqueue_and_log ~xt queue message =
    enqueue ~xt queue message;
    (* BAD: The printf could be executed many times! *)
    Printf.printf "sent %s" message
val enqueue_and_log : xt:'a Xt.t -> string queue -> string -> unit = <fun>
```

one should use
[`post_commit`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-post_commit)

```ocaml
# let enqueue_and_log ~xt queue message =
    enqueue ~xt queue message;
    Xt.post_commit ~xt @@ fun () ->
    Printf.printf "sent %s" message
val enqueue_and_log : xt:'a Xt.t -> string queue -> string -> unit = <fun>
```

to make sure that the message is only printed once after the transaction has
actually completed successfully.

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
type 'a node = Nil | Node of 'a * 'a node Loc.t
```

A queue is then a pair of pointers to the head and tail of a queue:

```ocaml
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

The queue implementation in this section is an example of using **Kcas** to
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

In **Kcas**, a _transaction_ is essentially a function that can be called to
prepare a specification of an operation or operations, in the form of a
_transaction log_, that can then be _attempted to be performed atomically_ by
the underlying k-CAS-n-CMP algorithm provided by **Kcas**.

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
succeeds or the transaction function raises an exception (other than
[`Later`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Retry/index.html#exception-Later)
or
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
      | [] -> raise Exit
      | xs ->
        if Xt.exchange ~xt queue.middle xs != [] then
          raise Exit
    in
    try Xt.commit { tx } with Exit -> ()
val back_to_middle : 'a queue -> unit = <fun>
```

Note that the above uses
[`exchange`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-exchange)
to optimistically record shared memory accesses and then uses the `Exit`
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

First of all, notice that `dequeue` only calls `back_to_middle queue` after
making sure that `queue.middle` and `queue.back` have not already been accessed
using
[`is_in_log`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-is_in_log).
If the call `back_to_middle queue` would be made after accessing `queue.middle`
or `queue.back`, then those accesses would be recorded in the transaction log
`xt` and the log would be inconsistent after `back_to_middle queue` mutates the
locations. This would cause the transaction attempt to fail and we want to avoid
such doomed attempts.

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
type ('k, 'v) basic_hashtbl = {
  size: int Loc.t;
  data: ('k * 'v Loc.t) list Loc.t array Loc.t;
}
```

The basic hash table constructor just allocates all the locations:

```ocaml
# let basic_hashtbl () = {
    size = Loc.make 0;
    data = Loc.make (Loc.make_array 4 []);
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
    let i = Stdlib.Hashtbl.hash key mod n in
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
# let rehash ~xt hashtbl new_capacity =
    let new_data = Loc.make_array new_capacity [] in
    Xt.exchange ~xt hashtbl.data new_data
    |> Array.iter @@ fun assoc_loc ->
       Xt.get ~xt assoc_loc
       |> List.iter @@ fun ((key, _) as bucket) ->
          let i = Stdlib.Hashtbl.hash key mod new_capacity in
          Xt.modify ~xt new_data.(i) (List.cons bucket)
val rehash : xt:'a Xt.t -> ('b, 'c) basic_hashtbl -> int -> unit = <fun>
```

But that involves reading all the bucket locations. Any mutation that adds or
removes an association would cause such a rehash to fail.

To avoid taking on such adversarial races, we can use a level of indirection:

```ocaml
type ('k, 'v) hashtbl = {
  pending: [`Nothing | `Rehash of int] Loc.t;
  basic: ('k, 'v) basic_hashtbl;
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
        let size =
          Int.max 1 (Xt.get ~xt hashtbl.basic.size + delta)
        and capacity =
          Array.length (Xt.get ~xt hashtbl.basic.data)
        in
        if capacity < size * 4 then
          Xt.set ~xt hashtbl.pending (`Rehash (capacity * 2))
        else if size * 8 < capacity then
          Xt.set ~xt hashtbl.pending (`Rehash (capacity / 2))
        else
          raise Exit
    in
    try
      if Xt.is_in_log ~xt hashtbl.pending then
        tx ~xt
      else
        Xt.commit { tx }
    with Exit -> ()
val prepare_rehash : xt:'a Xt.t -> ('b, 'c) hashtbl -> int -> unit = <fun>
```

Note again that while the rehash logic allows some slack in the capacity, a real
implementation would likely use a bigger minimum capacity and perhaps avoid
using powers of two. Also, if we have already modified the hash table, which we
know by using
[`is_in_log`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-is_in_log)
to check whether the `pending` location has been accessed, we must continue
within the same transaction.

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

### Avoid false sharing

[False sharing](https://en.wikipedia.org/wiki/False_sharing) is a form of
contention that arises when some location, that is being written to by at least
a single core, happens to be in memory next to &mdash; within the same cache
line aligned region of memory &mdash; another location that is accessed, read or
written, by other cores.

Perhaps contrary to how it is often described, false sharing doesn't require the
use of atomic variables or atomic instructions. Consider the following example:

```ocaml
# type state = { mutable counter : int; mutable finished: bool; }
type state = { mutable counter : int; mutable finished : bool; }

# let state = { counter = 1_000; finished = false }
val state : state = {counter = 1000; finished = false}

# let reader = Domain.spawn @@ fun () ->
    while not state.finished do
      Domain.cpu_relax ()
    done
val reader : unit Domain.t = <abstr>

# while 0 < state.counter do
    state.counter <- state.counter - 1
  done;
- : unit = ()

# state.finished <- true;
- : unit = ()

# Domain.join reader
- : unit = ()
```

The `state` is a record with two fields, `counter` and `finished`, next to each
other, which makes it rather likely for them to happen to reside in the same
cache line aligned region of memory. The main domain repeatedly mutates the
`counter` field and the other domain repeatedly reads the `finished` field. What
this means in practice is that the reads of the `finished` field by the other
domain will be very expensive, because the cache is repeatedly invalidated by
the `counter` updates done by the main domain.

The above example is contrived, of course, but this sort of false sharing can
happen very easily. Cache lines are typically relatively large &mdash; 8, 16, or
even 32 words wide. Typically many, if not most, heap allocated objects in OCaml
are smaller than a cache line, which means that false sharing may easily happen
even between seemingly unrelated objects.

To completely avoid false sharing one would basically need to make sure that
mutable locations (atomic or otherwise) are not allocated next to locations that
might be accessed from other domains. Unfortunately, that is difficult to
achieve without being expensive in itself as it tends to increase memory usage
and the amount of initializing stores.

The
[`Loc.make`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Loc/index.html#val-make)
function takes an optional `padded` argument, which can be explicitly specified
as `~padded:true` to request the location to be allocated in a way to avoid
false sharing. Using `~padded:true` on long lived shared memory locations that
are being repeatedly modified can improve performance significantly. Using
`~padded:true` on short lived shared memory locations is not recommended.

Using `~padded:true` does not eliminate all false sharing, however. Consider the
following sketch of a queue data structure:

```ocaml
type 'a queue = {
  head: 'a list Loc.t;
  tail: 'a list Loc.t;
}
```

Even if you allocate the locations with padding

```ocaml
# let queue () = {
    head = Loc.make ~padded:true [];
    tail = Loc.make ~padded:true [];
  }
val queue : unit -> 'a queue = <fun>
```

the queue record will still be vulnerable to the same kind of false sharing as
in the earlier example:

```ocaml
# let a_queue : int queue = queue ()
val a_queue : int queue = {head = <abstr>; tail = <abstr>}

# let counter = ref 1_000
val counter : int ref = {contents = 1000}
```

Above the reference cell for the `counter` might exhibit false sharing with the
queue record (which is read-only) and significantly degrade the performance of
the queue for passing messages between domains.

To avoid the above kind of problems, a strategic approach is to also allocate
the queue record in a way to avoid false sharing. Unfortunately OCaml does not
currently provide a standard way to do so. The
[multicore-magic](https://github.com/ocaml-multicore/multicore-magic) library
provides a function
[`copy_as_padded`](https://ocaml-multicore.github.io/multicore-magic/doc/multicore-magic/Multicore_magic/index.html#val-copy_as_padded)
for the purpose. Using
[`copy_as_padded`](https://ocaml-multicore.github.io/multicore-magic/doc/multicore-magic/Multicore_magic/index.html#val-copy_as_padded)
one would write

```ocaml
# let queue () =
    Multicore_magic.copy_as_padded {
      head = Loc.make ~padded:true [];
      tail = Loc.make ~padded:true [];
    }
val queue : unit -> 'a queue = <fun>
```

to allocate the queue record in a way to avoid false sharing.

Note that allocating long lived data structures, like queues, used for inter
domain communication in the way as described above to avoid false sharing does
not eliminate all false sharing, but it is likely to reduce false sharing
significantly with relatively low effort.

### Beware of torn reads

The algorithm underlying **Kcas** ensures that it is not possible to read
uncommitted changes to shared memory locations and that an operation can only
complete successfully if all of the accesses taken together were atomic. These
are very strong guarantees and make it much easier to implement correct
concurrent algorithms.

Unfortunately, the transaction mechanism that **Kcas** provides does not prevent
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

And create a helper that spawns a domain that repeatedly increments `a` and
decrements `b` in a transaction:

```ocaml
# let with_updater fn =
    let stop = ref false in
    let domain = Domain.spawn @@ fun () ->
      while not !stop do
        let tx ~xt =
          Xt.incr ~xt a;
          Xt.decr ~xt b
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
    for _ = 1 to 1_000 do
      let tx ~xt =
        0 = Xt.get ~xt a + Xt.get ~xt b
      in
      if not (Xt.commit { tx }) then
        failwith "read skew"
    done;
    "no read skew"
- : string = "no read skew"
```

Nice! So, it appears everything works as expected. A transaction can only commit
after having read a consistent, atomic, snapshot of all the shared memory
locations.

Unfortunately within a transaction attempt things are not as simple. Let's do an
experiment where we abort the transaction in case we observe that the values of
`a` and `b` are inconsistent:

```ml
# with_updater @@ fun () ->
    for _ = 1 to 1_000 do
      let tx ~xt =
        if 0 <> Xt.get ~xt a + Xt.get ~xt b then
          failwith "read skew"
      in
      Xt.commit { tx }
    done;
    "no read skew"
Exception: Failure "read skew".
```

Oops! So, within a transaction we may actually observe different locations
having values from different committed transactions. This is something that
needs to be kept in mind when writing transactions.

To mitigate issues due to read skew and to also avoid problems with long running
transactions, the **Kcas** transaction mechanism automatically validates the
transaction log periodically when an access is made to the transaction log.
Therefore an important guideline for writing transactions is that loops inside a
transaction should always include an access of some shared memory location
through the transaction log or should otherwise be guaranteed to be bounded.

In addition to the automatic periodic validation, one can also explicitly
[`validate`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html#val-validate),
_after_ reading some locations, that the locations have not been modified
outside of the transaction:

```ocaml
# with_updater @@ fun () ->
    for _ = 1 to 1_000 do
      let tx ~xt =
        let a' = Xt.get ~xt a in
        let b' = Xt.get ~xt b in
        Xt.validate ~xt a;
        if 0 <> a' + b' then
          failwith "read skew"
      in
      Xt.commit { tx }
    done;
    "no read skew"
- : string = "no read skew"
```

Notice that above we only validated the access of `a`, because we know that `a`
and `b` are always updated atomically and we read `b` after reading `a`. In this
case that is enough to ensure that read skew is not possible.
