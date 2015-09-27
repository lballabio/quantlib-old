
LDFLAGS = -lboost_serialization -lboost_system -lboost_filesystem

noinst_LTLIBRARIES = libExampleObjects.la

libExampleObjects_la_SOURCES =

noinst_HEADERS = \
    accountexample.hpp

libExampleObjects_la_LIBADD = \
    Enumerations/libEnumerations.la \
    Serialization/libSerialization.la \
    ValueObjects/libValueObjects.la

EXTRA_DIST = \
    ExampleObjects_vc8.vcproj \
    ExampleObjects_vc9.vcproj \
    ExampleObjects_vc10.vcxproj \
    ExampleObjects_vc10.vcxproj.filters \
    ExampleObjects_vc11.vcxproj \
    ExampleObjects_vc11.vcxproj.filters \
    ExampleObjects_vc12.vcxproj \
    ExampleObjects_vc12.vcxproj.filters

