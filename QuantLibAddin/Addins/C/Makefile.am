
AM_CPPFLAGS = -I${top_srcdir}

EXTRA_DIST = \
    AddinC.dsp \
    AddinC.vcproj \
    AddinC_vc8.vcproj

if BUILD_C
noinst_LTLIBRARIES = libQuantLibAddinC.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinC.la
endif

libQuantLibAddinC_la_LIBADD = ../../qla/libQuantLibAddin.la

if BUILD_C
noinst_HEADERS = \
    basic.h \
    capfloor.h \
    couponvectors.h \
    defines.h \
    instruments.h \
    interpolation.h \
    ohfunctions.h \
    options.h \
    processes.h \
    qladdin.h \
    schedule.h \
    shortratemodels.h \
    simpleswap.h \
    swap.h \
    termstructures.h \
    utilities.h \
    varies.h \
    varies.hpp \
    volatilities.h \
    xibor.h
endif

libQuantLibAddinC_la_SOURCES = \
    basic.cpp \
    capfloor.cpp \
    couponvectors.cpp \
    instruments.cpp \
    interpolation.cpp \
    ohfunctions.cpp \
    options.cpp \
    processes.cpp \
    schedule.cpp \
    session.cpp \
    shortratemodels.cpp \
    simpleswap.cpp \
    swap.cpp \
    termstructures.cpp \
    utilities.cpp \
    varies.cpp \
    volatilities.cpp \
    xibor.cpp

