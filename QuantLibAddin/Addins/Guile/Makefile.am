
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
    capfloor.h \
    couponvectors.h \
    extra.h \
    instruments.h \
    interpolation.h \
    objecthandler.h \
    options.h \
    processes.h \
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
    capfloor.cpp \
    couponvectors.cpp \
    guileutils.cpp \
    instruments.cpp \
    interpolation.cpp \
    objecthandler.cpp \
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

