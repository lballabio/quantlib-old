AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/oh

include_HEADERS = \
    exception.hpp \
    factory.hpp \
    logger.hpp \
    objecthandler.hpp \
    object.hpp \
    objhandlerdefines.hpp \
    objhandler.hpp \
    property.hpp \
    singleton.hpp \
    utilities.hpp

EXTRA_DIST = \
    autolink.hpp \
	makefile.mak

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -llog4cxx -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    exception.cpp  \
    logger.cpp  \
    object.cpp  \
    objecthandler.cpp  \
    utilities.cpp

