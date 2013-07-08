QT     -= core gui
CONFIG -= app_bundle
CONFIG -= qt
TEMPLATE = app
CONFIG += console
CONFIG += link_prl

macx {
    INCLUDEPATH += /opt/local/include /opt/local/include/boost
    LIBS += -L/opt/local/lib -lboost_timer-mt -lboost_timer-mt -lboost_thread-mt
}
macx {
    INCLUDEPATH += /Users/osx/Documents/QuantLib/quantlib/QuantLib/
    CONFIG(debug, debug|release) {
        #LIBS += -L/opt/local/lib/ -lQuantLib
        #LIBS += -L/Users/osx/Documents/QuantLib/quantlib/build-QuantLib-Desktop_Qt_5_0_2_clang_64bit-Debug/lib -lquantlibd
        LIBS += -L/Users/osx/Documents/QuantLib/quantlib/QuantLib/lib -lquantlibd
    } else {
        #LIBS += -L/opt/local/lib/ -lQuantLib
        #LIBS += -L/Users/osx/Documents/QuantLib/quantlib/build-QuantLib-Desktop_Qt_5_0_2_clang_64bit-Release/lib -lquantlib
        LIBS += -L/Users/osx/Documents/QuantLib/quantlib/QuantLib/lib -lquantlib
    }

}

SOURCES += main.cpp

