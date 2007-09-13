 
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/Cpp/libQuantLibAddinCpp.la
LDFLAGS = -lObjectHandler -lQuantLib -lboost_filesystem

EXTRA_DIST = \
    ClientCppSwapIn_vc8.vcproj \
    qlxl_demo_market.xml \
    qlxl_demo_swap.xml

swap_in_SOURCES = swap_in.cpp

if BUILD_CPP
noinst_PROGRAMS = swap_in
else
EXTRA_PROGRAMS = swap_in
endif

