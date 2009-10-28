.PHONY: all doc test check

all: doc

doc:
	@(cd doc ; make all)

test: check

check:
	@(cd test; $(MAKE) check)
