
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = SviSmileSection
TESTS = SviSmileSection$(EXEEXT)
else
noinst_PROGRAMS = SviSmileSection
endif
SviSmileSection_SOURCES = SviSmileSection.cpp
SviSmileSection_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: SviSmileSection$(EXEEXT)

check-examples: examples
	./SviSmileSection$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

