
AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/qlo/serialization

noinst_LTLIBRARIES = libSerialization.la

include_HEADERS = \
    processor.hpp \
    serializationfactory.hpp

libSerialization_la_SOURCES = \
    class_exports.cpp \
    processor.cpp \
    register_creators.cpp \
    serializationfactory.cpp

libSerialization_la_LIBADD = \
    create/libCreate.la \
    register/libRegister.la

