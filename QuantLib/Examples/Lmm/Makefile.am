
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = Lmm
TESTS = Lmm$(EXEEXT)
else
noinst_PROGRAMS = Lmm
endif
Lmm_SOURCES = Lmm.cpp
Lmm_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: Lmm$(EXEEXT)

check-examples: examples
	./Lmm$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

