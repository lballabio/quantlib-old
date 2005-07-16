
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../Addins/C/libQuantLibAddinC.la
LDFLAGS = -lObjectHandler -lQuantLib

EXTRA_DIST = \
    ClientCDemo.dsp \
    ClientCDemo_vc8.vcproj \
    ClientC.dev

QLADemo_SOURCES = qlademo.c
Options_SOURCES = options.c
Instruments_SOURCES = instruments.c

if BUILD_C
noinst_PROGRAMS = QLADemo Options Instruments
else
EXTRA_PROGRAMS = QLADemo Options Instruments
endif

