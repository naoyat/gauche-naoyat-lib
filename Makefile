.PHONY: all doc test check

all: doc

doc:
	@(cd doc ; make all)

test: check

check:
	@(cd test; $(MAKE) check)

LIBDIR = /usr/local/share/gauche/site/lib

install:
	if [ ! -d $(LIBDIR)/naoyat ]; then mkdir $(LIBDIR)/naoyat ; fi
	(cd ./naoyat && tar cf - .) | (cd $(LIBDIR)/naoyat && tar xfp - )

