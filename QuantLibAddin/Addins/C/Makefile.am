
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
#includedir = $(prefix)/include/qla

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
    options.h \
    qladdin.h \
    utilities.h \
    varies.h \
    varies.hpp

libQuantLibAddinC_la_SOURCES = \
    options.cpp \
    utilities.cpp \
    varies.cpp

