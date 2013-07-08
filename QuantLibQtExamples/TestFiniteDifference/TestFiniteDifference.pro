
QT      -= core gui
TEMPLATE = app
CONFIG  *= link_pri
CONFIG  -= qt

TARGET   = TestFiniteDifference
SOURCES += TestFiniteDifference.cpp \
           ../customutilities.cpp

HEADERS += ../customutilities.hpp

# Boost Library
macx {
    INCLUDEPATH += /opt/local/include /opt/local/include/boost
    LIBS += -L/opt/local/lib -lboost_timer-mt
}
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
        LIBS += -L/home/algefrontal/boost/boost_1_50_0/libd -lboost_timer-mt-d
    } else {
        LIBS += -L/home/algefrontal/boost/boost_1_50_0/lib -lboost_timer-mt
    }
}

# QuantLib
macx {
    INCLUDEPATH += ~/Documents/QuantLib/quantlib/QuantLib/
    CONFIG(debug, debug|release) {
        #LIBS += -L/opt/local/lib/ -lQuantLib
        LIBS += -L/Users/osx/Documents/QuantLib/quantlib/build-QuantLib-Desktop_Qt_5_0_2_clang_64bit-Debug/lib -lquantlibd
    } else {
        #LIBS += -L/opt/local/lib/ -lQuantLib
        LIBS += -L/Users/osx/Documents/QuantLib/quantlib/build-QuantLib-Desktop_Qt_5_0_2_clang_64bit-Release/lib -lquantlib
    }

}
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
