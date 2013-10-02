
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = BermudanSwaption2
TESTS = BermudanSwaption2$(EXEEXT)
else
noinst_PROGRAMS = BermudanSwaption2
endif
BermudanSwaption2_SOURCES = BermudanSwaption2.cpp
BermudanSwaption2_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: BermudanSwaption2$(EXEEXT)

check-examples: examples
	./BermudanSwaption2$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

