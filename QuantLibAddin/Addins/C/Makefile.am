
AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/qla

lib_LTLIBRARIES = libQuantLibAddinC.la
libQuantLibAddinC_la_LDFLAGS = -release $(PACKAGE_VERSION)

libQuantLibAddinC_la_LIBADD = ../../qla/libQuantLibAddin.la

libQuantLibAddinC_la_SOURCES = \
    options_c.cpp \
    utilities_c.cpp \
    varies.cpp

