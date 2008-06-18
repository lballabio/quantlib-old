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
    types.hpp \
    utilities.hpp \
    valueobject.hpp

EXTRA_DIST = \
    auto_link.hpp

lib_LTLIBRARIES = libObjectHandler.la
LDFLAGS = -llog4cxx -lboost_regex -lboost_serialization -release $(PACKAGE_VERSION)

libObjectHandler_la_SOURCES = \
    logger.cpp \
    repository.cpp \
    serializationfactory.cpp \
    utilities.cpp

libObjectHandler_la_LIBADD = \
    enumerations/libEnumerations.la

