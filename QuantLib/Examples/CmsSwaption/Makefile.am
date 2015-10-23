
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = CmsSwaption
TESTS = CmsSwaption$(EXEEXT)
else
noinst_PROGRAMS = CmsSwaption
endif
CmsSwaption_SOURCES = CmsSwaption.cpp
CmsSwaption_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: CmsSwaption$(EXEEXT)

check-examples: examples
	./CmsSwaption$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

