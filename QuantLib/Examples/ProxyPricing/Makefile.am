
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = ProxyPricing
TESTS = ProxyPricing$(EXEEXT)
else
noinst_PROGRAMS = ProxyPricing
endif
ProxyPricing_SOURCES = ProxyPricing.cpp
ProxyPricing_LDADD = ../../ql/libQuantLib.la

EXTRA_DIST =

.PHONY: examples check-examples

examples: ProxyPricing$(EXEEXT)

check-examples: examples
	./ProxyPricing$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

