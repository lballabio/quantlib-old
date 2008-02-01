 
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/Cpp/libQuantLibAddinCpp.la
LDFLAGS = -lObjectHandler -lQuantLib -lboost_filesystem

EXTRA_DIST = \
    ClientCppSwapOut_vc8.vcproj \
    qlxl_swap_market.xml \
    qlxl_swap_trade.xml

swap_out_SOURCES = swap_out.cpp

if BUILD_CPP
noinst_PROGRAMS = swap_out
else
EXTRA_PROGRAMS = swap_out
endif

