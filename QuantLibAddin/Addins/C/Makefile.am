
AM_CPPFLAGS = -I${top_srcdir}

EXTRA_DIST = \
    AddinC.dev \
    AddinC.dsp \
    AddinC.vcproj \
    AddinC_vc8.vcproj

# for now we do not install the C addin
noinst_LTLIBRARIES = libQuantLibAddinC.la

libQuantLibAddinC_la_LIBADD = ../../qla/libQuantLibAddin.la

# for now we do not install the C addin headers
noinst_HEADERS = \
    capfloor.h \
    defines.h \
    instruments.h \
    options.h \
    processes.h \
    shortratemodels.h \
    termstructures.h \
    qladdin.h \
    utilities.h \
    varies.h \
    varies.hpp \
    volatilities.h

libQuantLibAddinC_la_SOURCES = \
    capfloor.cpp \
    instruments.cpp \
    options.cpp \
    processes.cpp \
    shortratemodels.cpp \
    termstructures.cpp \
    utilities.cpp \
    varies.cpp \
    volatilities.cpp

