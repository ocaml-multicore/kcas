# Release notes

All notable changes to this project will be documented in this file.

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
