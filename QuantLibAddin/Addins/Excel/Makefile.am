
EXTRA_DIST = \
    AddinExcel.dev \
    AddinExcel.dsp \
    AddinExcel_vc8.vcproj \
    AddinExcel.vcproj \
    framewrk.cpp \
    framewrk.hpp \
    options.cpp \
    qladdin.cpp \
    utilities.cpp \
    xlcall.h \
    xlutils.cpp \
    xlutils.hpp

dist-hook:
	mkdir $(distdir)/lib
	cp -p $(srcdir)/lib/XLCALL32.LIB $(distdir)/lib

