AM_CPPFLAGS = -I${top_srcdir}

include_HEADERS = \
    config.hpp \
    exception.hpp \
    objecthandler.hpp \
    object.hpp \
    objhandlerdefines.hpp \
    objhandler.hpp \
    property.hpp \
    singleton.hpp \
    utilities.hpp

EXTRA_DIST = \
    autolink.hpp

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    exception.cpp  \
    object.cpp  \
    objecthandler.cpp  \
    utilities.cpp
