### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### To make a new release

1. Update [CHANGES.md](CHANGES.md).
2. Run `dune-release tag VERSION` to create a tag for the new `VERSION`.
3. Run `dune-release distrib` to create package locally.
4. Run `dune-release publish distrib` to create release on GitHub.
5. Run `opam publish --tag=VERSION` to create PR to
   [opam-repository](https://github.com/ocaml/opam-repository).
6. Run `./update-gh-pages-for-tag VERSION` to update the online documentation.
