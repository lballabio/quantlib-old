
CLEANFILES = quantlib_wrap.cpp QuantLib.pm .build-stamp \
QuantLib.bs pm_to_blib \
Makefile.quantlib

BUILT_SOURCES = quantlib_wrap.cpp QuantLib.pm

if HAVE_PYTHON

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES)
	$(PERL) Makefile.PL
	make -f Makefile.quantlib
	touch .build-stamp

check-local: .build-stamp
#	$(PERL) setup.pl test

install-exec-local: .build-stamp
#	$(PERL) setup.pl install

clean-local:
	rm -rf build

endif

quantlib_wrap.cpp QuantLib.pm: ../SWIG/*.i
	$(SWIG) -perl -c++ -I../SWIG  \
            -o quantlib_wrap.cpp quantlib.i

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.pl $(distdir)/examples
	mkdir -p $(distdir)/test
	cp ./test/*.pl $(distdir)/test

EXTRA_DIST = README.txt setup.pl $(BUILT_SOURCES)

