
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = KahaleSmileSection
TESTS = KahaleSmileSection$(EXEEXT)
else
noinst_PROGRAMS = KahaleSmileSection
endif
KahaleSmileSection_SOURCES = KahaleSmileSection.cpp
KahaleSmileSection_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: KahaleSmileSection$(EXEEXT)

check-examples: examples
	./KahaleSmileSection$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

