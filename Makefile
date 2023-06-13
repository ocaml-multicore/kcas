.PHONY: build test bench

all: build test bench

build:
	@dune build @install

test:
	@dune runtest --force

bench:
	@dune exec --release -- bench/main.exe 10

clean:
	@dune clean
