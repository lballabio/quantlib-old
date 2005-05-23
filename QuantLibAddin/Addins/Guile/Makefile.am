
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)

if BUILD_GUILE
lib_LTLIBRARIES = libQuantLibAddinGuile.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinGuile.la
endif

libQuantLibAddinGuile_la_LIBADD = ../../qla/libQuantLibAddin.la

noinst_HEADERS = \
    capfloor.h \
    extra.h \
    guileutils.hpp \
    instruments.h \
    options.h \
    processes.h \
    shortratemodels.h \
    termstructures.h \
    utilities.h \
    volatilities.h

libQuantLibAddinGuile_la_SOURCES = \
    capfloor.cpp \
    guileutils.cpp \
    instruments.cpp \
    options.cpp \
    processes.cpp \
    qladdin.c \
    shortratemodels.cpp \
    termstructures.cpp \
    utilities.cpp \
    volatilities.cpp

libQuantLibAddinGuile_la_LDFLAGS = \
    -lQuantLib -lObjectHandler -lguile

