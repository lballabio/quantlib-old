AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/oh

include_HEADERS = \
    config.hpp \
    exception.hpp \
    group.hpp \
    iless.hpp \
    libraryobject.hpp \
    logger.hpp \
    objecthandler.hpp \
    object.hpp \
    objectwrapper.hpp \
    observable.hpp \
    ohdefines.hpp \
    processor.hpp \
    property.hpp \
    range.hpp \
    repository.hpp \
    serializationfactory.hpp \
    singleton.hpp \
    utilities.hpp \
    valueobject.hpp

EXTRA_DIST = \
    auto_link.hpp

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -lboost_filesystem -lboost_regex -lboost_serialization -release $(PACKAGE_VERSION)
if OH_LINK_LOG4CXX
LDFLAGS += -llog4cxx
endif

libObjectHandler_la_SOURCES = \
    logger.cpp \
    processor.cpp \
    repository.cpp \
    serializationfactory.cpp \
    utilities.cpp

libObjectHandler_la_LIBADD = \
    enumerations/libEnumerations.la

