!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

unix {
    #QMAKE_CXXFLAGS += -std=c++0x
    #QMAKE_CXXFLAGS += -std=c++0x
}

SOURCES += TestInflation.cpp

