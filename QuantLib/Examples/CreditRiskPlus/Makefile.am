
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = CreditRiskPlus
TESTS = CreditRiskPlus$(EXEEXT)
else
noinst_PROGRAMS = CreditRiskPlus
endif
CreditRiskPlus_SOURCES = CreditRiskPlus.cpp
CreditRiskPlus_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: CreditRiskPlus$(EXEEXT)

check-examples: examples
	./CreditRiskPlus$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

