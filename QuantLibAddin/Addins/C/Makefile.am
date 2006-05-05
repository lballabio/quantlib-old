
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
    calendar.h \
    capfloor.h \
    couponvectors.h \
    date.h \
    daycounter.h \
    defines.h \
    exercise.h \
    instruments.h \
    interpolation.h \
    mathf.h \
    ohfunctions.h \
    options.h \
    prices.h \
    processes.h \
    qladdin.h \
    randomsequencegenerator.h \
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
    calendar.cpp \
    capfloor.cpp \
    couponvectors.cpp \
    date.cpp \
    daycounter.cpp \
    exercise.cpp \
    instruments.cpp \
    interpolation.cpp \
    mathf.cpp \
    ohfunctions.cpp \
    options.cpp \
    prices.cpp \
    processes.cpp \
    randomsequencegenerator.cpp \
    schedule.cpp \
    shortratemodels.cpp \
    simpleswap.cpp \
    swap.cpp \
    termstructures.cpp \
    utilities.cpp \
    varies.cpp \
    volatilities.cpp \
    xibor.cpp

