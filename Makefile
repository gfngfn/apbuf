
.PHONY: all
all: src/*
	dune build src/main.exe
	cp ./_build/default/src/main.exe ./apbuf
