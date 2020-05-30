
TEST_BAR_ELM_MODULE=Bar

.PHONY: all
all: build

.PHONY: build
build: src/*
	dune build src/apbuf.exe
	cp ./_build/default/src/apbuf.exe ./apbuf

.PHONY: install
install: src/*
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
test: test-bar

.PHONY: test-bar
test-bar:
	./apbuf examples/bar.txt
	cd examples/gen/elm && elm make src/$(TEST_BAR_ELM_MODULE).elm && cd ../../..
	cd examples/gen/play-scala-seed && sbt compile && cd ../../..
