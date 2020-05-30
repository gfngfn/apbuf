
TEST_BAR_ELM_MODULE=Bar

.PHONY: all
all: src/*
	dune build src/main.exe
	cp ./_build/default/src/main.exe ./apbuf

.PHONY: clean
clean:
	dune clean
	rm -f examples/gen/elm/src/*.elm
	rm -f examples/gen/play-scala-seed/app/assets/apbuf/*.scala

.PHONY: test
test: test-bar

.PHONY: test-bar
test-bar:
	./apbuf examples/bar.txt
	cd examples/gen/elm && elm make src/$(TEST_BAR_ELM_MODULE).elm && cd ../../..
	cd examples/gen/play-scala-seed && sbt compile && cd ../../..
