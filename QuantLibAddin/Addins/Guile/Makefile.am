
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qla/Addins/Guile

if BUILD_GUILE
lib_LTLIBRARIES = libQuantLibAddinGuile.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinGuile.la
endif

libQuantLibAddinGuile_la_LIBADD = ../../qla/libQuantLibAddin.la

if BUILD_GUILE
noinst_HEADERS = \
    calendar.h \
    capfloor.h \
    couponvectors.h \
    date.h \
    daycounter.h \
    exercise.h \
    extra.h \
    instruments.h \
    interpolation.h \
    mathf.h \
    ohfunctions.h \
    options.h \
    prices.h \
    processes.h \
    randomsequencegenerator.h \
    schedule.h \
    shortratemodels.h \
    simpleswap.h \
    swap.h \
    termstructures.h \
    utilities.h \
    volatilities.h \
    xibor.h

include_HEADERS = \
    guileutils.hpp
endif

if BUILD_GUILE
libQuantLibAddinGuile_la_SOURCES = \
    calendar.cpp \
    capfloor.cpp \
    couponvectors.cpp \
    date.cpp \
    daycounter.cpp \
    exercise.cpp \
    extra.cpp \
    guileutils.cpp \
    instruments.cpp \
    interpolation.cpp \
    mathf.cpp \
    ohfunctions.cpp \
    options.cpp \
    prices.cpp \
    processes.cpp \
    qladdin.c \
    randomsequencegenerator.cpp \
    schedule.cpp \
    session.cpp \
    shortratemodels.cpp \
    simpleswap.cpp \
    swap.cpp \
    termstructures.cpp \
    utilities.cpp \
    volatilities.cpp \
    xibor.cpp
endif

libQuantLibAddinGuile_la_LDFLAGS = \
    -lQuantLib -lQuantLibFunctions -lObjectHandler -lguile

