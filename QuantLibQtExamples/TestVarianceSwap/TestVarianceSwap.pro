QT      -= core gui
TEMPLATE = app
CONFIG  *= link_pri

# Use Precompiled headers (PCH)
CONFIG  *= precompile_header
PRECOMPILED_HEADER  = precompileheader.hpp
precompile_header:!isEmpty(PRECOMPILED_HEADER) {
    DEFINES += USING_PCH
}

#TARGET   = main
SOURCES += \
           customutilities.cpp \
    TestVarianceSwap.cpp
HEADERS += customutilities.hpp

# Boost Library
win32 {
    INCLUDEPATH += C:/boost/boost/boost_1_50_0
    CONFIG(debug, debug|release) {
        LIBS += C:/boost/boost_1_50_0/lib/x86/libboost_timer-vc100-mt-gd-1_50.lib
    } else {
        LIBS += C:/boost/boost_1_50_0/lib/x86/libboost_timer-vc100-mt-1_50.lib
    }
}
unix {
    INCLUDEPATH += /home/algefrontal/boost/boost_1_50_0
    CONFIG(debug, debug|release) {
        LIBS += -L/home/algefrontal/boost/boost_1_50_0/lib -lboost_timer-mt
    } else {
        LIBS += -L/home/algefrontal/boost/boost_1_50_0/lib -lboost_timer-mt
    }
}

# QuantLib
win32 {
    INCLUDEPATH += C:/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLib-vc100-mt-gd.lib
    } else {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLib-vc100-mt.lib
    }
}
unix {
    INCLUDEPATH += /home/algefrontal/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        LIBS += -L/home/algefrontal/QuantLib/QuantLibQt/lib -lQuantLibQtd
    } else {
        LIBS += -L/home/algefrontal/QuantLib/QuantLibQt/lib -lQuantLibQt
    }
}
