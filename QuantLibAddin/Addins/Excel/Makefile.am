
EXTRA_DIST = \
    AddinExcel.dev \
    AddinExcel.dsp \
    AddinExcel_vc8.vcproj \
    AddinExcel.vcproj \
    framewrk.cpp \
    framewrk.hpp \
    functions.cpp \
    options.cpp \
    qladdin.cpp \
    utilities.cpp \
    utilities.hpp \
    xlcall.h

dist-hook:
	mkdir $(distdir)/lib
	cp -p $(srcdir)/lib/XLCALL32.LIB $(distdir)/lib

