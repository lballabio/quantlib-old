
AM_CPPFLAGS = -I${top_srcdir} -I${top_srcdir}/Examples

LDADD = ../ExampleObjects/libExampleObjects.la \
        ../../oh/libObjectHandler.la
LDFLAGS = -lboost_filesystem -lboost_serialization -lboost_regex -lboost_system

EXTRA_DIST = \
    ExampleCpp_vc7.vcproj \
    ExampleCpp_vc8.vcproj \
    ExampleCpp_vc9.vcproj \
    ExampleCpp_vc10.vcxproj

ExampleCpp_SOURCES = example.cpp

noinst_PROGRAMS = ExampleCpp

