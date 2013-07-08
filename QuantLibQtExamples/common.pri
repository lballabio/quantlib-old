#Includes common configuration for all subdirectory .pro files.
INCLUDEPATH += . ..
#WARNINGS += -Wall

# project settings
QT     -= core gui
TEMPLATE = app
CONFIG -= app_bundle
CONFIG -= qt
CONFIG += console
CONFIG += link_prl


# Use Precompiled headers (PCH)
#CONFIG  += precompile_header
#PRECOMPILED_HEADER = ../precompileheader.hpp
##PRECOMPILED_HEADER = $$PWD/precompileheader.hpp
##PRECOMPILED_HEADER = C:/QuantLib/QuantLibQtExamples/precompileheader.hpp
#precompile_header:!isEmpty(PRECOMPILED_HEADER) {
#    DEFINES += USING_PCH
#}

SOURCES += ../customutilities.cpp

HEADERS += ../customutilities.hpp \
           ../precompileheader.hpp

gcc {
    message(gcc)
    QMAKE_CXXFLAGS += -std=c++11
    #QMAKE_CXXFLAGS += -pthread
    #LIBS += -pthread
}
macx-g++ {
    message(macx-g++)
}
macx-clang {
    message(macx-clang)
}

# The following keeps the generated files at least somewhat separate
# from the source files.
#UI_DIR = uics
#MOC_DIR = mocs
#OBJECTS_DIR = objs


# Boost Library
macx {
    INCLUDEPATH += /opt/local/include /opt/local/include/boost
    LIBS += -L/opt/local/lib -lboost_timer-mt -lboost_thread-mt
}
win32 {
    INCLUDEPATH += C:/boost/boost/boost_1_50_0
    CONFIG(debug, debug|release) {
        LIBS += C:/boost/boost_1_50_0/lib/x86/libboost_timer-vc100-mt-gd-1_50.lib
    } else {
        LIBS += C:/boost/boost_1_50_0/lib/x86/libboost_timer-vc100-mt-1_50.lib
    }
}
#unix {
#    #INCLUDEPATH += /home/algefrontal/boost/boost_1_49_0
#    #INCLUDEPATH += ../../boost/boost_1_49_0
#    #INCLUDEPATH += ../../boost/boost_1_50_0
#    INCLUDEPATH += /home/deriversatile/boost/boost_1_53_0
#    CONFIG(debug, debug|release) {
#        #LIBS += -L/home/algefrontal/boost/boost_1_49_0/lib -lboost_timer-mt
#        #LIBS += -L../../boost/boost_1_49_0/lib -lboost_timer-mt
#        #LIBS += -L../../../boost/boost_1_50_0/lib -lboost_timer-mt
#        LIBS += -L/home/deriversatile/boost/boost_1_53_0/lib -lboost_timer-mt-d
#    } else {
#        #LIBS += -L/home/algefrontal/boost/boost_1_49_0/lib -lboost_timer-mt
#        #LIBS += -L~/boost/boost_1_49_0/lib -lboost_timer-mt
#        #LIBS += -L../../boost/boost_1_49_0/lib -lboost_timer-mt
#        #LIBS += -L../../../boost/boost_1_50_0/lib -lboost_timer-mt
#        LIBS += -L/home/deriversatile/boost/boost_1_53_0/lib -lboost_timer-mt
#    }
#}

# QuantLib
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
win32 {
    INCLUDEPATH += C:/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLib-vc100-mt-gd.lib
    } else {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLib-vc100-mt.lib
    }
}
#unix {
#    #INCLUDEPATH += /home/algefrontal/QuantLib/QuantLibQt
#    #INCLUDEPATH += ../QuantLibQt
#    #INCLUDEPATH += /home/deriversatile/QuantLibXPS14ub/QuantLibQt
#    INCLUDEPATH += /home/deriversatile/QuantLib/QuantLibQt
#    CONFIG(debug, debug|release) {
#        #LIBS += -L/home/algefrontal/QuantLib/QuantLibQt/lib -lQuantLibQtd
#        #LIBS += -L../../QuantLibQt/lib -lQuantLibQtd
#        #LIBS += -L/home/deriversatile/QuantLibXPS14ub/QuantLibQt/lib -lQuantLibQtd
#        LIBS += -L/home/deriversatile/QuantLib/QuantLibQt/lib -lQuantLibQtd
#    } else {
#        #LIBS += -L/home/algefrontal/QuantLib/QuantLibQt/lib -lQuantLibQt
#        #LIBS += -L~/QuantLib/QuantLibQt/lib -lQuantLibQt
#        #LIBS += -L../../QuantLibQt/lib -lQuantLibQt
#        #LIBS += -L/home/deriversatile/QuantLibXPS14ub/QuantLibQt/lib -lQuantLibQt
#        LIBS += -L/home/deriversatile/QuantLib/QuantLibQt/lib -lQuantLibQt
#    }
#}
#win32:QMAKE_CXXFLAGS_DEBUG += /Warning:disable(4819)
win32-msvc* {
    #QMAKE_CXXFLAGS += /wd4819 /wd4101 /wd4102 /wd4189 /wd4996
    QMAKE_CXXFLAGS += /wd4819
}


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
message(PWD: $$PWD)
message(LIBS: $$LIBS)
