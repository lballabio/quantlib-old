
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)

if BUILD_GUILE
lib_LTLIBRARIES = libQuantLibAddinGuile.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinGuile.la
endif

libQuantLibAddinGuile_la_LIBADD = ../../qla/libQuantLibAddin.la

if BUILD_GUILE
noinst_HEADERS = \
    capfloor.h \
    couponvectors.h \
    extra.h \
    guileutils.hpp \
    instruments.h \
    options.h \
    processes.h \
    shortratemodels.h \
    schedule.h \
    simpleswap.h \
    swap.h \
    termstructures.h \
    utilities.h \
    volatilities.h \
    xibor.h
endif

if BUILD_GUILE
libQuantLibAddinGuile_la_SOURCES = \
    capfloor.cpp \
    couponvectors.cpp \
    guileutils.cpp \
    instruments.cpp \
    options.cpp \
    processes.cpp \
    qladdin.c \
    schedule.cpp \
    shortratemodels.cpp \
    simpleswap.cpp \
    swap.cpp \
    termstructures.cpp \
    utilities.cpp \
    volatilities.cpp \
    xibor.cpp
endif

libQuantLibAddinGuile_la_LDFLAGS = \
    -lQuantLib -lObjectHandler -lguile

