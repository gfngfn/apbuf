
.PHONY: all
all: src/*
	dune build src/main.exe
