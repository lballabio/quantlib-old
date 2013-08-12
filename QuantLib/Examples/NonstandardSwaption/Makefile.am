
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = NonstandardSwaption
TESTS = NonstandardSwaption$(EXEEXT)
else
noinst_PROGRAMS = NonstandardSwaption
endif
NonstandardSwaption_SOURCES = NonstandardSwaption.cpp
NonstandardSwaption_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: NonstandardSwaption$(EXEEXT)

check-examples: examples
	./NonstandardSwaption$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

