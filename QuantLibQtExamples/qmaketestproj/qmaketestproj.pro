#Includes common configuration for all subdirectory .pro files.
#INCLUDEPATH += . ..
WARNINGS += -Wall

# project settings
QT      -= core gui
TEMPLATE = app
CONFIG  *= link_prl


# Use Precompiled headers (PCH)
CONFIG  *= precompile_header
PRECOMPILED_HEADER = ../precompileheader.hpp
#PRECOMPILED_HEADER = $$PWD/precompileheader.hpp
#PRECOMPILED_HEADER = C:/QuantLib/QuantLibQtExamples/precompileheader.hpp
precompile_header:!isEmpty(PRECOMPILED_HEADER) {
    DEFINES += USING_PCH
}


# The following keeps the generated files at least somewhat separate
# from the source files.
#UI_DIR = uics
#MOC_DIR = mocs
#OBJECTS_DIR = objs


# Boost Library
win32-msvc* {
    INCLUDEPATH += C:/boost/boost_1_51_0
    CONFIG(debug, debug|release) {
        LIBS += C:/boost/boost_1_51_0/lib/libboost_timer-vc100-mt-gd-1_51.lib
    } else {
        LIBS += C:/boost/boost_1_51_0/lib/libboost_timer-vc100-mt-1_51.lib
    }
}
win32-g++ {
    INCLUDEPATH += C:/boost/boost_1_53_0
    CONFIG(debug, debug|release) {
        #LIBS += C:/boost/boost_1_53_0/lib/libboost_timer-mgw47-mt-d-1_53.a
        LIBS += -LC:/boost/boost_1_53_0/lib -lboost_timer-mgw47-mt-d-1_53
    } else {
        #LIBS += C:/boost/boost_1_53_0/lib/libboost_timer-mgw47-mt-1_53.a
        LIBS += -LC:/boost/boost_1_53_0/lib -lboost_timer-mgw47-mt-1_53
    }
}

unix {
    #INCLUDEPATH += /home/algefrontal/boost/boost_1_50_0
    INCLUDEPATH += /home/deriversatile/boost/boost_1_53_0
    CONFIG(debug, debug|release) {
        #LIBS += -L/home/algefrontal/boost/boost_1_50_0/libd -lboost_timer-mt-d
        LIBS += -L/home/deriversatile/boost/boost_1_53_0/lib -lboost_timer-mt-d
    } else {
        #LIBS += -L/home/algefrontal/boost/boost_1_50_0/lib -lboost_timer-mt
        LIBS += -L/home/deriversatile/boost/boost_1_53_0/lib -lboost_timer-mt
    }
}


# QuantLib
win32-msvc* {
    INCLUDEPATH += C:/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLibQtd.lib
    } else {
        LIBS += C:/QuantLib/QuantLibQt/lib/QuantLibQt.lib
    }
}
win32-g++ {
    INCLUDEPATH += C:/QuantLib/QuantLibQt
    CONFIG(debug, debug|release) {
        #LIBS += C:/QuantLib/QuantLibQt/lib/libQuantLibQtd.a
        LIBS += C:/QuantLib/QuantLibQt/lib -lQuantLibQtd
    } else {
        #LIBS += C:/QuantLib/QuantLibQt/lib/libQuantLibQt.a
        LIBS += C:/QuantLib/QuantLibQt/lib -lQuantLibQt
    }
}

unix {
    #INCLUDEPATH += /home/algefrontal/QuantLib/QuantLibQt
    #INCLUDEPATH += ../QuantLibQt
    #INCLUDEPATH += /home/deriversatile/QuantLibXPS14ub/QuantLibQt
    INCLUDEPATH += /home/deriversatile/QuantLib/QuantLibQt
    #LIBS += -LC:/QuantLib/QuantLibQt/lib
    CONFIG(debug, debug|release) {
        LIBS += -L/home/deriversatile/QuantLib/QuantLibQt/lib -lQuantLibQtd
    } else {
        LIBS += -L/home/deriversatile/QuantLib/QuantLibQt/lib -lQuantLibQt
    }
}

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


SOURCES += FiniteDifference.cpp \
           ../customutilities.cpp
HEADERS += customutilities.hpp \
           ../precompileheader.hpp

