AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/oh

include_HEADERS = \
    config.hpp \
    exception.hpp \
    logger.hpp \
    objecthandler.hpp \
    object.hpp \
    objhandlerdefines.hpp \
    objhandler.hpp \
    ohthing.hpp \
    singleton.hpp \
    utilities.hpp \
    valueobject.hpp

EXTRA_DIST = \
    auto_link.hpp

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -llog4cxx -lboost_regex -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    exception.cpp  \
    logger.cpp  \
    object.cpp  \
    objecthandler.cpp  \
    ohthing.cpp  \
    utilities.cpp

libObjectHandler_la_LIBADD = \
    ValueObjects/libValueObjects.la

