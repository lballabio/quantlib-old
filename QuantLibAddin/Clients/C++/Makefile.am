
AM_CPPFLAGS = -I${top_srcdir}
LDADD = ../../qlo/libQuantLibAddin.la ../../Addins/C++/libQuantLibAddinCpp.la
LDFLAGS = -lObjectHandler -lQuantLib

EXTRA_DIST = \
    ClientCppDemo_vc7.vcproj \
    ClientCppDemo_vc8.vcproj \
    set_fixings.hpp

#QLADemo_SOURCES = qlademo.cpp
cms_in_SOURCES = cms_in.cpp set_fixings.cpp
#test_deserialize_SOURCES = test_deserialize.cpp

if BUILD_CPP
#noinst_PROGRAMS = QLADemo cms_in test_deserialize
noinst_PROGRAMS = cms_in
else
#EXTRA_PROGRAMS = QLADemo cms_out test_deserialize
EXTRA_PROGRAMS = cms_out
endif

