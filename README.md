kcas — Multi-word compare-and-swap library
-------------------------------------------------------------------------------
%%VERSION%%

kcas is TODO

kcas is distributed under the ISC license.

Homepage: https://github.com/kayceesrk/kcas  

## Installation

kcas can be installed with `opam`:

    opam install kcas

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is automatically generated by from
the source interfaces. It can be consulted [online][doc].

[doc]: https://kayceesrk.github.io/kcas/doc

## Sample programs

If you installed kcas with `opam` sample programs are located in
the directory `opam config var kcas:doc`.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. They can be built and run
with:

    topkg build --tests true && topkg test 
