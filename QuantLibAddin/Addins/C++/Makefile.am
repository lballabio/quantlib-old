
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo/Addins/C++

if BUILD_CPP
lib_LTLIBRARIES = libQuantLibAddinCpp.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinCpp.la
endif

libQuantLibAddinCpp_la_LIBADD = ../../qlo/libQuantLibAddin.la

if BUILD_CPP
noinst_HEADERS = \
    exercise.hpp
endif

if BUILD_CPP
libQuantLibAddinCpp_la_SOURCES = \
    exercise.cpp
endif

libQuantLibAddinCpp_la_LDFLAGS = \
    -lQuantLib -lObjectHandler

