PERL_MAKEFILE = Makefile.quantlib

BUILT_SOURCES = quantlib_wrap.cpp QuantLib.pm

CLEANFILES = $(BUILT_SOURCES) $(PERL_MAKEFILE) .build-stamp

if HAVE_PERL

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES) $(PERL_MAKEFILE)
	make -f $(PERL_MAKEFILE)
	touch .build-stamp

check-local: .build-stamp
	make -f $(PERL_MAKEFILE) test

install-exec-local: .build-stamp
	make -f $(PERL_MAKEFILE) install

clean-local: $(PERL_MAKEFILE)
	make -f $(PERL_MAKEFILE) clean
	rm -f $(PERL_MAKEFILE).old
	rm -rf build

$(PERL_MAKEFILE):
	$(PERL) Makefile.PL

endif

quantlib_wrap.cpp QuantLib.pm: ../SWIG/*.i
	$(SWIG) -perl -c++ -I../SWIG  \
            -o quantlib_wrap.cpp quantlib.i

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt Makefile.PL $(BUILT_SOURCES)

