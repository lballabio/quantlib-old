
AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/qla/objects

#EXTRA_DIST = makefile.mak

include_HEADERS = \
    asianoption.hpp \
    barrieroption.hpp \
    options.hpp \
    optionutils.hpp \
    stochasticprocess.hpp \
    vanillaoption.hpp

libObjects_la_SOURCES = \
    asianoption.cpp \
    barrieroption.cpp \
    optionutils.cpp \
    stochasticprocess.cpp \
    vanillaoption.cpp

noinst_LTLIBRARIES = libObjects.la

