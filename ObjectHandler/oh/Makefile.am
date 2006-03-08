AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/oh

include_HEADERS = \
    config.hpp \
    exception.hpp \
    logger.hpp \
    objecthandlerbase.hpp \
    objecthandler.hpp \
    object.hpp \
    objhandlerdefines.hpp \
    objhandler.hpp \
    singleton.hpp \
    utilities.hpp \
    valueobject.hpp

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

