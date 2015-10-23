
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = SimulatedAnnealing
TESTS = SimulatedAnnealing$(EXEEXT)
else
noinst_PROGRAMS = SimulatedAnnealing
endif
SimulatedAnnealing_SOURCES = SimulatedAnnealing.cpp
SimulatedAnnealing_LDADD = ../../ql/libQuantLib.la

#EXTRA_DIST =

.PHONY: examples check-examples

examples: SimulatedAnnealing$(EXEEXT)

check-examples: examples
	./SimulatedAnnealing$(EXEEXT)

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build

