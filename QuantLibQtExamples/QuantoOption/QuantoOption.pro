!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

SOURCES += testquantooption.cpp \
           ../customutilities.cpp

HEADERS += ../customutilities.hpp

