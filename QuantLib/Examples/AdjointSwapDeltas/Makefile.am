AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

if AUTO_EXAMPLES
bin_PROGRAMS = AdjointSwapDeltas
TESTS = AdjointSwapDeltas$(EXEEXT)
else
noinst_PROGRAMS = AdjointSwapDeltas
endif
AdjointSwapDeltas_SOURCES = adjoint.cpp
AdjointSwapDeltas_LDADD = ../../ql/libQuantLib.la

EXTRA_DIST = \
	AdjointSwapDeltas.vcxproj \
	AdjointSwapDeltas.vcxproj.filters
#    Gaussian1dModels.dev
#    Gaussian1dModels_vc8.vcproj
#    Gaussian1dModels_vc9.vcproj

.PHONY: examples check-examples

examples: AdjointSwapDeltas$(EXEEXT)

check-examples: examples
	./AdjointSwapDeltas$(EXEEXT) --batch

dist-hook:
	mkdir -p $(distdir)/bin
	mkdir -p $(distdir)/build
