
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = MultitenorVols
TESTS = MultitenorVols$(EXEEXT)
else
noinst_PROGRAMS = MultitenorVols
endif
MultitenorVols_SOURCES = MultitenorVols.cpp
MultitenorVols_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: MultitenorVols$(EXEEXT)

check-examples: examples
	./MultitenorVols$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

