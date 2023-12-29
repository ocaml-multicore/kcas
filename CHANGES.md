# Release notes

All notable changes to this project will be documented in this file.

Next version:

- Numerous minor internal improvements (@polytypic)
- Made `Accumulator` automatically scaling and removed optional `n_way`
  arguments (@polytypic)
- Use polymorphic variant for `mode` (@polytypic)
- Add `?backoff` to `Loc.compare_and_set` (@polytypic)
- Remove the Op API (@polytypic, @lyrm)
- Fix `Hashtbl.clear` (@polytypic)
- Fix single location updates to be linearizable (@polytypic)
- Add `Xt.compare_and_set` (@polytypic)
- Add `Dllist.create_node value` (@polytypic)
- Workarounds for CSE optimization (@polytypic)
- Changed to use `(implicit_transitive_deps false)` (@polytypic)
- Move `Backoff` module to its own `backoff` package (@lyrm, @polytypic)
- Support padding to avoid false sharing (@polytypic)
- Pass through `?timeoutf` to blocking operations on data structures
  (@polytypic)
- Ported to OCaml 4.13 (@polytypic)

## 0.6.1

- Ported to OCaml 4.14 (@polytypic)

## 0.6.0

- Add timeout support to potentially blocking operations (@polytypic)
- Add explicit `~xt` parameter to `Xt.call` to make it polymorphic (@polytypic)

## 0.5.3

- Fix to also snapshot and rollback post commit actions (@polytypic)
- Fix `Loc.compare_and_set` to have strong semantics (@polytypic)
- Fix single location no-op updates to be strictly serializable (@polytypic)
- Add `Dllist.move_l node list` and `Dllist.move_r node list` (@polytypic)

## 0.5.2

- Improve `Hashtbl` read-write performance and add `swap` (@polytypic)
- Avoid some unnecessary verifies of read-only CMP operations (@polytypic)

## 0.5.1

- Add synchronizing variable `Mvar` to `kcas_data` (@polytypic)
- Fix to allow retry from within `Xt.update` and `Xt.modify` (@polytypic)

## 0.5.0

- Add nested conditional transaction support (@polytypic)
- Add explicit location validation support (@polytypic)

## 0.4.0

- Allocation of location ids in a transaction log friendly order (@polytypic)
- Per location operating mode selection (@Dashy-Dolphin, review: @polytypic)
- Injectivity `!'a Kcas_data.Dllist.t` annotation (@polytypic)

## 0.3.1

- Added doubly-linked list `Dllist` to `kcas_data` (@polytypic)
- Minor optimizations (@polytypic)

## 0.3.0

- Remove the `Tx` API (@polytypic)
- Add blocking support to turn kcas into a proper STM (@polytypic, review:
  @lyrm)
- Add periodic validation of transactions (@polytypic)

## 0.2.4

- Introduce `kcas_data` companion package of composable lock-free data
  structures (@polytypic)
- Add `is_in_log` operation to determine whether a location has been accessed by
  a transaction (@polytypic)
- Add `Loc.modify` (@polytypic)
- Add transactional `swap` operation to exchange contents of two locations
  (@polytypic)
- Injectivity `!'a Loc.t` and variance `+'a Tx.t` annotations (@polytypic)

## 0.2.3

- Add support for post commit actions to transactions (@polytypic)
- Bring `Xt` and `Tx` access combinators to parity and add `compare_and_swap`
  (@polytypic)

## 0.2.2

- New explicit transaction log passing API based on idea by @gasche (@polytypic,
  review: @samoht and @lyrm)

## 0.2.1

- New k-CAS-n-CMP algorithm extending the GKMZ algorithm (@polytypic, review:
  @bartoszmodelski)

## 0.2.0

- Complete redesign adding a new transaction API (@polytypic, review:
  @bartoszmodelski)

## 0.1.8

- Fix a bug in GKMZ implementation (@polytypic, review: @bartoszmodelski)

## 0.1.7

- Change to use the new GKMZ algorithm (@polytypic, review: @bartoszmodelski)

## 0.1.6

- Add preflights sorting and checks (@bartoszmodelski, review: @polytypic)

## 0.1.5

- Republish in opam (update opam, dune) (@tmcgilchrist, review: @Sudha247)
