

all: test


test:
	(cd src && racket conv_xml.rkt)
	(cd src && racket question2.rkt)
	(cd src && racket qu3.rkt)
