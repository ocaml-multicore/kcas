.PHONY: all build test bench

all: build 

build:
	@dune build @install

test:
	@dune runtest --force

clean:
	@dune clean

bench:
	@dune exec -- ./bench/main.exe
