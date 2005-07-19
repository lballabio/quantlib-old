
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qla/libQuantLibAddin.la
LDFLAGS = -lObjectHandler -lQuantLib

noinst_PROGRAMS = QLADemo Options Instruments CapFloor

#    ClientCPP.dev \
EXTRA_DIST = \
    ClientCPPCapFloor.dsp \
    ClientCPPCapFloor_vc8.vcproj \
    ClientCPPDemo.dsp \
    ClientCPPDemo_vc8.vcproj \
    ClientCPPInstruments.dsp \
    ClientCPPInstruments_vc8.vcproj \
    ClientCPPOptions.dsp \
    ClientCPPOptions_vc8.vcproj

QLADemo_SOURCES = qlademo.cpp
Options_SOURCES = options.cpp
Instruments_SOURCES = instruments.cpp
CapFloor_SOURCES = capfloor.cpp

