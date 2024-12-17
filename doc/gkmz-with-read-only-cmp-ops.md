# Extending k-CAS with efficient read-only CMP operations

> **_NOTE_**: This document was originally written at around the time the kcas
> library was extended with a
> [`Tx`](https://ocaml-multicore.github.io/kcas/0.2.0/kcas/Kcas/Tx/index.html)
> API for monadic transactions. This version of the document has been updated to
> use the new
> [`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html)
> API.

[`Kcas`](https://ocaml-multicore.github.io/kcas/) currently uses the GKMZ
algorithm for
[Efficient Multi-word Compare and Swap](https://arxiv.org/abs/2008.02527) or
MCAS aka k-CAS. This is a nearly optimal algorithm for MCAS as it requires only
`k + 1` CAS operations.

The new library API also provides a transactional API for using the algorithm.
For example, suppose one would create the following shared memory locations:

<!--
```ocaml
# #require "kcas"
# open Kcas
```
-->

```ocaml
let a = Loc.make 10
let b = Loc.make 52
let x = Loc.make 0
let y = Loc.make 0
```

Using the
[`Xt`](https://ocaml-multicore.github.io/kcas/doc/kcas/Kcas/Xt/index.html) API
one could define a transaction `x_to_b_sub_a`

```ocaml
let x_to_b_sub_a ~xt =
  let a' = Xt.get ~xt a
  and b' = Xt.get ~xt b in
  Xt.set ~xt x (b' - a')
```

to update `x` with the difference of `b` and `a` and commit that transaction:

```ocaml
Xt.commit { tx = x_to_b_sub_a }
```

One could similarly define a transaction `y_to_a_add_b`

```ocaml
let y_to_a_add_b ~xt =
  let a' = Xt.get ~xt a
  and b' = Xt.get ~xt b in
  Xt.set ~xt y (a' + b')
```

to update `y` with the sum of `a` and `b` and commit that transaction:

```ocaml
Xt.commit { tx = y_to_a_add_b }
```

The above committed transactions essentially correspond to MCAS operations as
follows:

```ml
Xt.commit { tx = x_to_b_sub_a } == [ CAS (a, 10, 10); CAS (b, 52, 52); CAS (x, 0, 42) ]
Xt.commit { tx = y_to_a_add_b } == [ CAS (a, 10, 10); CAS (b, 52, 52); CAS (y, 0, 62) ]
```

CAS with equal expected or before and desired or after values essentially
expresses an operation that does not change the logical content of the target
location, but only "asserts" that it does not change during the operation.

Note that the transactions `x_to_b_sub_a` and `y_to_a_add_b`, unlike the MCAS
operations they generate, are independent of the exact values of the locations
being accessed. It is important to distinguish between them. A transaction is a
specification for generating a list of CASes.

One might attempt to perform both of the two MCAS operations

```ml
[ CAS (a, 10, 10); CAS (b, 52, 52); CAS (x, 0, 42) ]
```

and

```ml
[ CAS (a, 10, 10); CAS (b, 52, 52); CAS (y, 0, 62) ]
```

in parallel, but that will not be allowed by the GKMZ algorithm. Every CAS
actually updates the targeted memory locations. This means two things:

1. CAS operations targeting the same location can only execute sequentially.
2. CAS operations, even those that do not change the logical content of a
   location, cause contention as after the operation only the cache of the
   writer will have a valid copy of the
   [shared memory](https://en.wikipedia.org/wiki/MSI_protocol) location.

Could we extend upon GKMZ and allow read-only CMP operations to be expressed
directly and also make it so that read-only CMP operations do not write to
memory?

Let's first examine, in a bit more detail, how GKMZ, or our OCaml adaptation of
it, operates. For this issue, in order to make it a bit easier to follow, I'll
simplify the implementation a bit. We'll replace the splay tree representation
with a simpler list, assume that locations are always given in some total order,
and we also ignore the release of unused values after the operation.

Here are the core data structures used by the GKMZ algorithm:

```ocaml
 type 'a loc = 'a state Atomic.t
 and 'a state = { before : 'a; after : 'a; casn : casn }
 and cass = CASS : 'a loc * 'a state -> cass
 and casn = status Atomic.t
 and status = Undetermined of cass list | After | Before
```

Take a closer look at the internal `cass` type. Previously we explained
transactions and talked about MCAS in terms a different CAS type whose
definition could look like this:

```ocaml
 type cas =
   | CAS : 'a loc * 'a * 'a -> cas
```

The difference between the above logical `cas` operation and the internal `cass`
descriptor is that the logical `cas` deals with plain values of type `'a` while
the internal `cass` descriptor uses a state type or the `'a state` type. A
location `'a loc` is an atomic location that contains a `'a state`. The core
GKMZ algorithm attempts a MCAS by attempting to replace the states of all the
locations in a list of `cass`es and then setting status of the `casn` descriptor
to either `After` or `Before` depending on whether the whole operation was a
success or a failure, respectively.

When using GKMZ one first prepares a list of internal `cass` descriptors and a
`casn` descriptor that has the `Undetermined` list of those descriptors. The
data structure is cyclic: `casn` contains the list of `cass` descriptors which
contain `state`s which contain a reference to the `casn` descriptor. This cyclic
form allows the whole data structure to be traversed starting from any `state`,
which one might find in a location.

Here is the core of the GKMZ algorithm in OCaml:

```ocaml
 let finish casn desired =
   match Atomic.get casn with
   | After -> true
   | Before -> false
   | Undetermined _ as current ->
     Atomic.compare_and_set casn current desired |> ignore;
     Atomic.get casn == After

 let rec gkmz casn = function
   | [] -> finish casn After (* seems like a success *)
   | (CASS (loc, desired) :: continue) as retry ->
     let current = Atomic.get loc in
     if desired == current then
       gkmz casn continue
     else
       let current_value =
         if is_after current.casn then
           current.after
         else
           current.before
       in
       if current_value != desired.before then
         finish casn Before (* seems like a failure *)
       else
         match Atomic.get casn with
         | Undetermined _ ->
           (* operation still unfinished *)
           if Atomic.compare_and_set loc current desired then
             gkmz casn continue
           else
             gkmz casn retry
         | After -> true (* operation was a success *)
         | Before -> false (* operation was a failure *)

 and is_after casn =
   match Atomic.get casn with
   | Undetermined cass -> gkmz casn cass
   | After -> true
   | Before -> false

 let get loc =
   let state = Atomic.get loc in
   if is_after state.casn then
     state.after
   else
     state.before

 let atomically logical_cas_list =
   let casn = Atomic.make After in
   let cass =
     logical_cas_list
     |> List.map @@ function
        | CAS (loc, before, after) -> CASS (loc, {before; after; casn})
   in
   Atomic.set casn (Undetermined cass);
   gkmz casn cass
```

Note that every call of `atomically` allocates a fresh location for a `casn`
descriptor and also fresh `state`s for all the `CASS` descriptors. This is
important as it makes sure that
[ABA problems](https://en.wikipedia.org/wiki/ABA_problem) are avoided. It is
doubly important in the following extended algorithm.

Let's then simply extend the algorithm to allow `CMP` operations. First we
extend the logical `cas` type with a new logical `CMP` operation:

```diff
 type cas =
   | CAS : 'a loc * 'a * 'a -> cas
+  | CMP : 'a loc * 'a -> cas
```

It turns out that we don't need to change the internal data structures at all.
The gist is that an internal read-only `CASS (loc, state)` descriptor refers to
the `state` of a location before the operation started. We can distinguish such
a `state` simply by comparing the `casn` of the state to the `casn` of the
entire operation. Furthermore, because we know that the `state`s and `casn` are
always freshly allocated, we know that we can compare them simply by their
identities without [ABA problems](https://en.wikipedia.org/wiki/ABA_problem).

Then we extend the algorithm to allow read-only `CMP` operations. The idea is
simple: instead of attempting to store the `state`s of read-only `CMP`
operations to the locations, we simply check that those locations have their
original `state`s. Additionally, before we attempt to complete an operation as a
success (by writing `After` to the `casn`), we verify that all of the read-only
locations still have their original values.

```diff
+let is_cmp casn state =
+  state.casn != casn
+
 let finish casn desired =
   match Atomic.get casn with
   | After -> true
   | Before -> false
-  | Undetermined _ as current ->
+  | Undetermined cass as current ->
+    let desired =
+       if desired == After
+          && cass
+             |> List.exists @@ fun (CASS (loc, state)) ->
+                is_cmp casn state && Atomic.get loc != state then
+          Before
+       else
+          desired in
     Atomic.compare_and_set casn current desired |> ignore;
     Atomic.get casn == After

 let rec gkmz casn = function
   | [] -> finish casn After (* seems like a success *)
   | (CASS (loc, desired) :: continue) as retry ->
     let current = Atomic.get loc in
     if desired == current then
       gkmz casn continue
+    else if is_cmp casn desired then
+      finish casn Before (* seems like a failure *)
     else
       let current_value =
         if is_after current.casn then
           current.after
         else
           current.before
       in
       if current_value != desired.before then
         finish casn Before (* seems like a failure *)
       else
         match Atomic.get casn with
         | Undetermined _ ->
           (* operation still unfinished *)
           if Atomic.compare_and_set loc current desired then
             gkmz casn continue
           else
             gkmz casn retry
         | After -> true (* operation was a success *)
         | Before -> false (* operation was a failure *)

 and is_after casn =
   match Atomic.get casn with
   | Undetermined cass -> gkmz casn cass
   | After -> true
   | Before -> false

 let get loc =
   let state = Atomic.get loc in
   if is_after state.casn then
     state.after
   else
     state.before

 let atomically logical_cas_list =
   let casn = Atomic.make After in
   let cass =
     logical_cas_list
     |> List.map @@ function
        | CAS (loc, before, after) -> CASS (loc, {before; after; casn})
+       | CMP (loc, expected) ->
+         let current = Atomic.get loc in
+         if get loc != expected || Atomic.get loc != current then
+           raise Exit
+         else
+           CASS (loc, current)
   in
   Atomic.set casn (Undetermined cass);
   gkmz casn cass
+
+let atomically logical_cas_list =
+  try atomically logical_cas_list with Exit -> false
```

The above implementation is specifically designed to minimize the diffs compared
to the original GKMZ algorithm. Minor optimizations are possible that are not
shown above. Additionally, it makes sense to distinguish the case when the
algorithm specifically fails during the verification step. We'll get back to
this shortly.

We claim that the above algorithm is
[linearizable](https://cs.brown.edu/~mph/HerlihyW90/p463-herlihy.pdf) and
[obstruction-free](https://core.ac.uk/download/pdf/9590574.pdf).

It should be clear that if one thread runs the above algorithm in isolation, it
will be able to finish in a finite number of steps. When not running in
isolation, the verification steps (in `finish`) of two operations may
indefinitely cause both to fail. To see this, consider the following operations
operating in parallel:

```ml
[ CMP (a, 0); CAS (b, 0, 1) ] and [ CAS (a, 0, 1); CMP (b, 0) ]
```

Let's assume both operations manage to initially convert the `CMP` operations in
`atomically`, check the read-only `CASS` once during `gkmz`, and perform their
mutating `CASS` operations. At that point both enter the verification step and
both of them will fail. The same could happen on a subsequent retry.

To prove linearizability we need to show that, for any set of operations
performed in parallel, there is an order in which the operations could have been
performed sequentially giving the same state at the end for all locations.

First note that the new algorithm operates exactly the same as the original GKMZ
algorithm in case only `CAS` operations are performed. All the cases where
operations write to overlapping locations are already proven to be linearizable
by the basic GKMZ algorithm. Operations that are completely non-overlapping are
trivially linearizable.

The interesting case to consider is when an operation `R` that only reads a
location `x` (and might also write other locations) needs to be linearizable
with an operation `W` that writes to said location `x`. We claim any observer of
said operations will only be able to read an end state that is consistent with
`R` happening before `W`. Let's assume the opposite, that an observer reads the
results of `W` and `R` and can determine that the operation `W` happened before
`R`. For that to be possible, the `casn` descriptors of both `W` and `R` must be
set to the `After` state. The only way for that to be possible is that the `R`
operation verified the `x` location before `W` wrote to it and so the result of
`R` must be consistent with `R` happening before `W`. This contradicts the
assumption and proves the original claim.

Previously we mentioned that it makes sense to distinguish the case when the
verification step fails. Let's assume we have done so. Consider having a
transaction mechanism using the new algorithm. Initially such a mechanism
attempts to perform the transaction optimistically using the obstruction-free
algorithm for `k-CAS-n-CMP`. If that repeatedly fails during the verification
step, then the transaction mechanism can switch to using only `k-CAS` operations
and try to complete the operation in lock-free manner. This way the transaction
mechanism can guarantee lock-free behavior, which ensures that at least one
thread will be able to make progress.

Recall the example transactions `x_to_b_sub_a` and `y_to_a_add_b` that we
started with. Using the new `k-CAS-n-CMP` algorithm the transactions can
generate the following operations:

```ml
Xt.commit { tx = x_to_b_sub_a } == [ CMP (a, 10); CMP (b, 52); CAS (x, 0, 42) ]
Xt.commit { tx = y_to_a_add_b } == [ CMP (a, 10); CMP (b, 52); CAS (y, 0, 62) ]
```

The new algorithm will then be able to run the two transaction in parallel.
