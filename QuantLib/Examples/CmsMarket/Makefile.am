
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = CmsMarket
TESTS = CmsMarket$(EXEEXT)
else
noinst_PROGRAMS = CmsMarket
endif
CmsMarket_SOURCES = CmsMarket.cpp
CmsMarket_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: CmsMarket$(EXEEXT)

check-examples: examples
	./CmsMarket$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

