NAOYAT_LIBRARY = ..
TESTRECORD = ./test.record

.PHONY: all check test-all

.SUFFIXES: .scm .test
.scm.test:
	@GAUCHE_TEST_RECORD_FILE=$(TESTRECORD) \
	gosh -I$(NAOYAT_LIBRARY) $<

all:

check:
	@rm -f *.test $(TESTRECORD)
	make test-all
	@cat $(TESTRECORD)

test-all: printf.test random.test matrix.test erf.test speech.test

speech.test: speech.scm
	@yes | GAUCHE_TEST_RECORD_FILE=$(TESTRECORD) \
	gosh -I$(NAOYAT_LIBRARY) speech.scm
