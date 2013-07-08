!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

SOURCES += FiniteDifference.cpp \
           customutilities.cpp
HEADERS += customutilities.hpp

