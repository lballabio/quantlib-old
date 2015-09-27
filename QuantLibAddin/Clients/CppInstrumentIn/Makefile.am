 
instrument_in_CPPFLAGS = -I${top_srcdir}
instrument_in_LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/Cpp/libQuantLibAddinCpp.la
instrument_in_LDFLAGS = -lObjectHandler -lQuantLib -lboost_filesystem -lboost_serialization -lboost_system -lboost_regex

EXTRA_DIST = \
    CppInstrumentIn_vc8.vcproj \
    CppInstrumentIn_vc9.vcproj \
    CppInstrumentIn_vc10.vcxproj \
    CppInstrumentIn_vc11.vcxproj \
    CppInstrumentIn_vc12.vcxproj \
    price_Swap.bat \
    price_Swap.sh

instrument_in_SOURCES = instrument_in.cpp

if BUILD_CPP
noinst_PROGRAMS = instrument_in
else
EXTRA_PROGRAMS = instrument_in
endif

dist-hook:
	mkdir -p $(distdir)/MarketData
	mkdir -p $(distdir)/Models
	cp -p $(srcdir)/MarketData/*.xml $(distdir)/MarketData
	cp -p $(srcdir)/Models/*.xml $(distdir)/Models

