!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

TARGET   = LiborMarketModel
HEADERS += ../customutilities.hpp

SOURCES += LiborMarketModel.cpp \
           ../customutilities.cpp

