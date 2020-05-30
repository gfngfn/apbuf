
TEST_BAR_ELM_MODULE=Bar
TEST_FOO_ELM_MODULE=Foo

.PHONY: all
all: build

.PHONY: build
build: src/*
	rm -f ./apbuf
	dune build src/apbuf.exe
	cp ./_build/default/src/apbuf.exe ./apbuf

.PHONY: install
install: src/* build
	dune build -p apbuf @install
	dune install

.PHONY: clean
clean:
	dune clean
	rm -f ./apbuf

.PHONY: clean-test
clean-test:
	rm -f examples/gen/elm/src/*.elm
	rm -f examples/gen/play-scala-seed/app/assets/apbuf/*.scala

.PHONY: test
test: test-bar test-foo

.PHONY: test-bar
test-bar: install
	apbuf examples/bar.txt
	cd examples/gen/elm && elm make src/$(TEST_BAR_ELM_MODULE).elm && cd ../../..
	cd examples/gen/play-scala-seed && sbt compile && cd ../../..

.PHONY: test-foo
test-foo: install
	apbuf examples/foo.txt
	cd examples/gen/elm && elm make src/$(TEST_FOO_ELM_MODULE).elm && cd ../../..
	cd examples/gen/play-scala-seed && sbt compile && cd ../../..
