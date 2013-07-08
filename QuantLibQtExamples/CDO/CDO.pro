QT      -= core gui
TEMPLATE = app
CONFIG  *= link_pri

TARGET   = CDO
SOURCES += CDO.cpp

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

#win32:QMAKE_CXXFLAGS_DEBUG += /Warning:disable(4819)


EVERYTHING = $$SOURCES $$HEADERS
message("The project contains the following files:")
message($$EVERYTHING)


message(Qt version: $$[QT_VERSION])
message(Qt is installed in $$[QT_INSTALL_PREFIX])
message(Qt resources can be found in the following locations:)
message(Documentation: $$[QT_INSTALL_DOCS])
message(Header files: $$[QT_INSTALL_HEADERS])
message(Libraries: $$[QT_INSTALL_LIBS])
message(Binary files (executables): $$[QT_INSTALL_BINS])
message(Plugins: $$[QT_INSTALL_PLUGINS])
message(Data files: $$[QT_INSTALL_DATA])
message(Translation files: $$[QT_INSTALL_TRANSLATIONS])
message(Settings: $$[QT_INSTALL_SETTINGS])
message(Examples: $$[QT_INSTALL_EXAMPLES])
message(Demonstrations: $$[QT_INSTALL_DEMOS])
