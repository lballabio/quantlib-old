
AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/qla

SUBDIRS = objects functions

lib_LTLIBRARIES = libQuantLibAddin.la
libQuantLibAddin_la_LDFLAGS = -release $(PACKAGE_VERSION)

libQuantLibAddin_la_LIBADD = \
    objects/libObjects.la \
    functions/libFunctions.la

libQuantLibAddin_la_SOURCES =

