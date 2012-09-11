
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo/Addins/Guile

if BUILD_GUILE
lib_LTLIBRARIES = libQuantLibAddinGuile.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinGuile.la
endif

libQuantLibAddinGuile_la_LIBADD = ../../qlo/libQuantLibAddin.la

if BUILD_GUILE
noinst_HEADERS = \
    conversions.hpp \
    exercise.h \
    extra.h \
    guileutils.hpp \
    instruments.h \
    ohfunctions.h \
    options.h \
    payoffs.h \
    pricingengines.h \
    processes.h \
    settings.h \
    volatilities.h
endif

if BUILD_GUILE
libQuantLibAddinGuile_la_SOURCES = \
    conversions.cpp \
    exercise.cpp \
    extra.cpp \
    guileutils.cpp \
    instruments.cpp \
    ohfunctions.cpp \
    options.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    qladdin.c \
    session.cpp \
    settings.cpp \
    volatilities.cpp
endif

libQuantLibAddinGuile_la_LDFLAGS = \
    -lQuantLib -lObjectHandler -lguile

