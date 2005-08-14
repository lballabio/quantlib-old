
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qla/libQuantLibAddin.la
LDFLAGS = -lObjectHandler -lQuantLib

noinst_PROGRAMS = QLADemo Options Instruments CapFloors

#    ClientCPP.dev
EXTRA_DIST = \
    ClientCPPCapFloors.dsp \
    ClientCPPCapFloors.vcproj \
    ClientCPPCapFloors_vc8.vcproj \
    ClientCPPDemo.dsp \
    ClientCPPDemo.vcproj \
    ClientCPPDemo_vc8.vcproj \
    ClientCPPInstruments.dsp \
    ClientCPPInstruments.vcproj \
    ClientCPPInstruments_vc8.vcproj \
    ClientCPPOptions.dsp \
    ClientCPPOptions.vcproj \
    ClientCPPOptions_vc8.vcproj

QLADemo_SOURCES = qlademo.cpp
Options_SOURCES = options.cpp
Instruments_SOURCES = instruments.cpp
CapFloors_SOURCES = capfloor.cpp

