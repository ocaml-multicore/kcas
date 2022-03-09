all: build test

build:
	@dune build @install

test:
	@dune runtest --force

clean:
	@dune clean
