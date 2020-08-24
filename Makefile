
.PHONY: test

all: test

build:
	(cd src && racket *.rkt)

test:
	racket test/all-tests.rkt
