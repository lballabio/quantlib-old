
AM_CPPFLAGS = -I${top_srcdir}

EXTRA_DIST = \
    AddinC.dev \
    AddinC.dsp \
    AddinC_vc8.vcproj \
    AddinC.vcproj

# for now we do not install the C addin
noinst_LTLIBRARIES = libQuantLibAddinC.la

libQuantLibAddinC_la_LIBADD = ../../qla/libQuantLibAddin.la

# for now we do not install the C addin headers
noinst_HEADERS = \
    defines.h \
    instruments.h \
    options.h \
    qladdin.h \
    utilities.h \
    varies.h \
    varies.hpp

libQuantLibAddinC_la_SOURCES = \
    instruments.cpp \
    options.cpp \
    utilities.cpp \
    varies.cpp

