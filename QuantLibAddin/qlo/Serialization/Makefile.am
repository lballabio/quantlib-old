
AM_CPPFLAGS = -I${top_srcdir}
includedir = $(prefix)/include/qlo/Serialization

noinst_LTLIBRARIES = libSerialization.la

include_HEADERS = \
    serializationfactory.hpp

libSerialization_la_SOURCES = \
    register_creators.cpp \
    serializationfactory.cpp

libSerialization_la_LIBADD = \
    Create/libCreate.la \
    Register/libRegister.la

