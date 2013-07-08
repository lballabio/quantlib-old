TEMPLATE = app
CONFIG += console
CONFIG -= qt

SOURCES += AlternatingDirectionImplicitGSL.cpp

unix {
   INCLUDEPATH += /usr/include/gsl
   LIBS += -L/usr/lib -lgsl
   LIBS += -L/usr/lib -lgslcblas
}

