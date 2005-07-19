
AM_CPPFLAGS = -I${top_srcdir}

#    AddinC.dev
EXTRA_DIST = \
    AddinC.dsp \
    AddinC_vc8.vcproj \
    AddinC.vcproj

if BUILD_C
noinst_LTLIBRARIES = libQuantLibAddinC.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinC.la
endif

libQuantLibAddinC_la_LIBADD = ../../qla/libQuantLibAddin.la

if BUILD_C
noinst_HEADERS = \
    capfloor.h \
    defines.h \
    instruments.h \
    options.h \
    processes.h \
    qladdin.h \
    shortratemodels.h \
    simpleswap.h \
    termstructures.h \
    utilities.h \
    varies.h \
    varies.hpp \
    volatilities.h \
    xibor.h
endif

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

