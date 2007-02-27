
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qlo/libQuantLibAddin.la
LDFLAGS = -lObjectHandler -lQuantLib

noinst_PROGRAMS = QLADemo #Options Instruments CapFloors

EXTRA_DIST = \
    ClientCppDemo.vcproj \
    ClientCppDemo_vc8.vcproj

QLADemo_SOURCES = qlademo.cpp
#Options_SOURCES = options.cpp
#Instruments_SOURCES = instruments.cpp
#CapFloors_SOURCES = capfloor.cpp

