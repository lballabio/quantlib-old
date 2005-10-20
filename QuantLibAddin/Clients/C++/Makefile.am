
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qla/libQuantLibAddin.la
LDFLAGS = -lObjectHandler -lQuantLib

noinst_PROGRAMS = QLADemo Options Instruments CapFloors

EXTRA_DIST = \
    ClientCppCapFloors.dsp \
    ClientCppCapFloors.vcproj \
    ClientCppCapFloors_vc8.vcproj \
    ClientCppDemo.dsp \
    ClientCppDemo.vcproj \
    ClientCppDemo_vc8.vcproj \
    ClientCppInstruments.dsp \
    ClientCppInstruments.vcproj \
    ClientCppInstruments_vc8.vcproj \
    ClientCppOptions.dsp \
    ClientCppOptions.vcproj \
    ClientCppOptions_vc8.vcproj

QLADemo_SOURCES = qlademo.cpp
Options_SOURCES = options.cpp
Instruments_SOURCES = instruments.cpp
CapFloors_SOURCES = capfloor.cpp

