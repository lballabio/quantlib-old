
AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/qla/objects

#EXTRA_DIST = makefile.mak

include_HEADERS = \
    asianoption.hpp \
    barrieroption.hpp \
    basketoption.hpp \
    cliquetoption.hpp \
    dividendvanillaoption.hpp \
    forwardvanillaoption.hpp \
    options.hpp \
    optionutils.hpp \
    stochasticprocess.hpp \
    vanillaoption.hpp

libObjects_la_SOURCES = \
    asianoption.cpp \
    barrieroption.cpp \
    basketoption.cpp \
    cliquetoption.cpp \
    dividendvanillaoption.cpp \
    forwardvanillaoption.cpp \
    optionutils.cpp \
    stochasticprocess.cpp \
    vanillaoption.cpp

noinst_LTLIBRARIES = libObjects.la

