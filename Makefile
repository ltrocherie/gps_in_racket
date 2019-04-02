

all: test

build:
	(cd src && racket *.rkt)

test:
	racket tst/all-tests.rkt
