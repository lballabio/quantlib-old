AM_CPPFLAGS = -I. -I${top_srcdir}
AM_LDFLAGS = -lboost_regex
LDADD = ../ObjectHandler/libObjectHandler.a

EXTRA_DIST = \
    Example.dev \
    Example.dsp \
    Example.vcproj \
    Example_8.vcproj \
    makefile.mak

example_SOURCES = \
    example.cpp  \
    interface.cpp  \
    objectwidget.cpp

noinst_PROGRAMS = example
