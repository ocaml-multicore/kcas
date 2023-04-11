FROM ocaml/opam:debian-ocaml-5.1
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
WORKDIR bench-dir
RUN opam remote add origin https://opam.ocaml.org && \
    opam update
COPY *.opam ./
RUN opam pin -yn --with-version=dev .
RUN opam install -y --deps-only --with-test .
COPY . ./
RUN opam exec -- dune build --release bench/main.exe