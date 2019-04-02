

all: test

build:
	(cd src && racket *.rkt)

test:
	(cd src && racket *tests.rkt)
