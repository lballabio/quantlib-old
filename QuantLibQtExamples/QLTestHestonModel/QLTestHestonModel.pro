!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

#TARGET   = main
SOURCES += main.cpp \
           customutilities.cpp
HEADERS += customutilities.hpp

