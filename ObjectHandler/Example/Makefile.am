AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -lboost_regex
LDADD = ../ObjectHandler/libObjectHandler.la

EXTRA_DIST = \
    Example.dev \
    Example.dsp \
    Example.vcproj \
    Example_vc8.vcproj \
    makefile.mak \
    interface.hpp \
    objectwidget.hpp \
    widget.hpp

Example_SOURCES = \
    example.cpp  \
    interface.cpp  \
    objectwidget.cpp

noinst_PROGRAMS = Example
