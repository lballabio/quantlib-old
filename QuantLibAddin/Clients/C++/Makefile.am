
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qla/libQuantLibAddin.la
LDFLAGS = -lObjectHandler -lQuantLib

#    makefile.mak
EXTRA_DIST = \
    ClientCPP.dev \
    ClientCPP.dsp \
    ClientCPP_vc8.vcproj \
    ClientCPP.vcproj

QLADemo_SOURCES = qlademo.cpp
Options_SOURCES = options.cpp
Instruments_SOURCES = instruments.cpp

noinst_PROGRAMS = QLADemo Options Instruments

