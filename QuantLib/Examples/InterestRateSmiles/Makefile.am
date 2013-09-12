
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = InterestRateSmiles
TESTS = InterestRateSmiles$(EXEEXT)
else
noinst_PROGRAMS = InterestRateSmiles
endif
InterestRateSmiles_SOURCES = InterestRateSmiles.cpp
InterestRateSmiles_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: InterestRateSmiles$(EXEEXT)

check-examples: examples
	./InterestRateSmiles$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

