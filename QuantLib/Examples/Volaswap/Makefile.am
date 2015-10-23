
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = Volaswap
TESTS = Volaswap$(EXEEXT)
else
noinst_PROGRAMS = Volaswap
endif
Volaswap_SOURCES = Volaswap.cpp
Volaswap_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: Volaswap$(EXEEXT)

check-examples: examples
	./Volaswap$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

