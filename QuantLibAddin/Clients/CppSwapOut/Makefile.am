 
swap_out_CPPFLAGS = -I${top_srcdir}
swap_out_LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/Cpp/libQuantLibAddinCpp.la
swap_out_LDFLAGS = -lObjectHandler -lQuantLib -lboost_filesystem

EXTRA_DIST = \
    ClientCppSwapOut_vc7.vcproj \
    ClientCppSwapOut_vc8.vcproj \
    ClientCppSwapOut_vc9.vcproj \
    YieldCurves.xml \
    VanillaSwap.xml

swap_out_SOURCES = swap_out.cpp

if BUILD_CPP
noinst_PROGRAMS = swap_out
else
EXTRA_PROGRAMS = swap_out
endif

