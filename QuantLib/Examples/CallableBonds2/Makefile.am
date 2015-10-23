
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = CallableBonds2
TESTS = CallableBonds2$(EXEEXT)
else
noinst_PROGRAMS = CallableBonds2
endif
CallableBonds2_SOURCES = CallableBonds2.cpp
CallableBonds2_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: CallableBonds2$(EXEEXT)

check-examples: examples
	./CallableBonds2$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

