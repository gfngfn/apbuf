
.PHONY: all
all: bar

.PHONY: init_scala
init_scala: bar
	sbt new playframework/play-scala-seed.g8

.PHONY: bar
bar: bar.txt
	mkdir -p gen
	../apbuf bar.txt
