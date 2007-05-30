
AM_CPPFLAGS = -I${top_srcdir}

EXTRA_DIST = \
    AddinC_vc7.vcproj \
    AddinC_vc8.vcproj

if BUILD_C
noinst_LTLIBRARIES = libQuantLibAddinC.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinC.la
endif

libQuantLibAddinC_la_LIBADD = ../../qlo/libQuantLibAddin.la

if BUILD_C
noinst_HEADERS = \
    auto_link.h \
    conversions.hpp \
    defines.h \
    exercise.h \
    instruments.h \
    ohfunctions.h \
    options.h \
    payoffs.h \
    pricingengines.h \
    processes.h \
    qladdin.h \
    settings.h \
    varies.h \
    varies.hpp \
    volatilities.h
endif

libQuantLibAddinC_la_SOURCES = \
    conversions.cpp \
    exercise.cpp \
    instruments.cpp \
    ohfunctions.cpp \
    options.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    settings.cpp \
    varies.cpp \
    volatilities.cpp

