AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/oh

include_HEADERS = \
    config.hpp \
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

#    makefile.mak
EXTRA_DIST = \
    autolink.hpp

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -llog4cxx -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    exception.cpp  \
    logger.cpp  \
    object.cpp  \
    objecthandler.cpp  \
    utilities.cpp

