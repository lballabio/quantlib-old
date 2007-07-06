
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/C++/libQuantLibAddinCpp.la
LDFLAGS = -lObjectHandler -lQuantLib

EXTRA_DIST = \
    ClientCppDemo_vc7.vcproj \
    ClientCppDemo_vc8.vcproj

QLADemo_SOURCES = qlademo.cpp
test_deserialize_SOURCES = test_deserialize.cpp

if BUILD_CPP
noinst_PROGRAMS = QLADemo test_deserialize
else
EXTRA_PROGRAMS = QLADemo test_deserialize
endif

