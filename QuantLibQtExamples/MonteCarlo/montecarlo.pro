!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

unix {
    #QMAKE_CXXFLAGS += -std=c++0x
    #QMAKE_CXXFLAGS += -std=c++0x
}


#TARGET   = TestVolatilityCube
SOURCES += montecarlo.cpp \
           ../customutilities.cpp

HEADERS += ../customutilities.hpp \
           ../precompileheader.hpp

