!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}

# Use Precompiled headers (PCH)
CONFIG  *= precompile_header
PRECOMPILED_HEADER  = precompileheader.hpp
precompile_header:!isEmpty(PRECOMPILED_HEADER) {
    DEFINES += USING_PCH
}

#TARGET   = Bonds
SOURCES += QLMBS.cpp \
    mortgagebackedsecurity.cpp
HEADERS += \
    mortgagebackedsecurity.hpp

!include(../common.pri) {
    error("Couldn't find the common.pri file!")
}
