
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qla

SUBDIRS = objects functions

#EXTRA_DIST = 
include_HEADERS = \
    autolink.hpp \
    qladdindefines.hpp \
    qladdin.hpp

lib_LTLIBRARIES = libQuantLibAddin.la

libQuantLibAddin_la_LIBADD = \
    objects/libObjects.la \
    functions/libFunctions.la

libQuantLibAddin_la_SOURCES =

