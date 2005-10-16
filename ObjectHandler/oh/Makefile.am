AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/oh

include_HEADERS = \
    config.hpp \
    exception.hpp \
    logger.hpp \
    object.hpp \
    objecthandler.hpp \
    objecthandlerbase.hpp \
    objhandler.hpp \
    objhandlerdefines.hpp \
    property.hpp \
    singleton.hpp \
    utilities.hpp

EXTRA_DIST = \
    autolink.hpp

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -llog4cxx -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    exception.cpp  \
    logger.cpp  \
    object.cpp  \
    objecthandlerbase.cpp  \
    utilities.cpp

