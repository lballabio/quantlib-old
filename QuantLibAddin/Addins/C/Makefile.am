
AM_CPPFLAGS = -I${top_srcdir}

EXTRA_DIST = \
    AddinC.dev \
    AddinC.dsp \
    AddinC.vcproj \
    AddinC_vc8.vcproj

if BUILD_C
noinst_LTLIBRARIES = libQuantLibAddinC.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinC.la
endif

libQuantLibAddinC_la_LIBADD = ../../qla/libQuantLibAddin.la

# for now we do not install the C addin headers
noinst_HEADERS = \
    capfloor.h \
    defines.h \
    instruments.h \
    options.h \
    processes.h \
    shortratemodels.h \
    simpleswap.h \
    termstructures.h \
    qladdin.h \
    utilities.h \
    varies.h \
    varies.hpp \
    volatilities.h \
    xibor.h

libQuantLibAddinC_la_SOURCES = \
    capfloor.cpp \
    instruments.cpp \
    options.cpp \
    processes.cpp \
    shortratemodels.cpp \
    simpleswap.cpp \
    termstructures.cpp \
    utilities.cpp \
    varies.cpp \
    volatilities.cpp \
    xibor.cpp

