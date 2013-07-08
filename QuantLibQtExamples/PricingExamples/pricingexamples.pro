!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

## Use Precompiled headers (PCH)
#CONFIG  += precompile_header
##PRECOMPILED_HEADER = ../precompileheader.hpp
#PRECOMPILED_HEADER = $$PWD/../precompileheader.hpp
#precompile_header:!isEmpty(PRECOMPILED_HEADER) {
#    DEFINES += USING_PCH
#    message(Using the precompiled header...)
#}

SOURCES += pricingexamples.cpp \
    cmstest.cpp \
    fdhestontest.cpp \
    jumpdiffusiontest.cpp

HEADERS += \
    cmstest.hpp \
    fdhestontest.hpp \
    jumpdiffusiontest.hpp
