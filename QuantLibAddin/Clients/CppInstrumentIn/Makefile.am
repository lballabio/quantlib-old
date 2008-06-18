 
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/Cpp/libQuantLibAddinCpp.la
LDFLAGS = -lObjectHandler -lQuantLib -lboost_filesystem

EXTRA_DIST = \
    CppInstrumentIn_vc7.vcproj \
    CppInstrumentIn_vc8.vcproj \
    CppInstrumentIn_vc9.vcproj \
    YieldCurves.xml \
    AtmVolatilities.xml \
    SwaptionVolatilities.xml \
    VanillaSwap.xml \
    ConstantMaturitySwap.xml

instrument_in_SOURCES = instrument_in.cpp

if BUILD_CPP
noinst_PROGRAMS = instrument_in
else
EXTRA_PROGRAMS = instrument_in
endif

