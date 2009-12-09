 
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -lObjectHandler -lQuantLib -lboost_filesystem
QLADemo_LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/Cpp/libQuantLibAddinCpp.la

EXTRA_DIST = \
    ClientCppDemo_vc7.vcproj \
    ClientCppDemo_vc8.vcproj \
    ClientCppDemo_vc9.vcproj

QLADemo_SOURCES = qlademo.cpp

if BUILD_CPP
noinst_PROGRAMS = QLADemo
else
EXTRA_PROGRAMS = QLADemo
endif

