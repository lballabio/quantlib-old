AM_CPPFLAGS = -I${top_srcdir}

include_HEADERS = \
    exception.hpp \
    objecthandler.hpp \
    object.hpp \
    objhandlerdefines.hpp \
    objhandler.hpp \
    property.hpp \
    singleton.hpp \
    utilities.hpp

lib_LTLIBRARIES = libObjectHandler.la
#libObjectHandler_la_LDFLAGS = -release $(PACKAGE_VERSION)
LDFLAGS = -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    exception.cpp  \
    object.cpp  \
    objecthandler.cpp  \
    utilities.cpp
