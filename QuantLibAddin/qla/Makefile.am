
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qla

#makefile.mak
EXTRA_DIST = \
    autolink.hpp

include_HEADERS = \
    asianoption.hpp \
    autolink.hpp \
    barrieroption.hpp \
    basketoption.hpp \
    cliquetoption.hpp \
    dividendvanillaoption.hpp \
    fixedcouponbond.hpp \
    forwardvanillaoption.hpp \
    generalutils.hpp \
    instruments.hpp \
    instrumentutils.hpp \
    options.hpp \
    optionutils.hpp \
    qladdindefines.hpp \
    qladdin.hpp \
    stochasticprocess.hpp \
    utilities.hpp \
    vanillaoption.hpp \
    zerocouponbond.hpp \
    zerocurve.hpp

lib_LTLIBRARIES = libQuantLibAddin.la

libQuantLibAddin_la_SOURCES = \
    asianoption.cpp \
    barrieroption.cpp \
    basketoption.cpp \
    cliquetoption.cpp \
    dividendvanillaoption.cpp \
    fixedcouponbond.cpp \
    forwardvanillaoption.cpp \
    generalutils.cpp \
    instrumentutils.cpp \
    optionutils.cpp \
    stochasticprocess.cpp \
    utilities.cpp \
    vanillaoption.cpp \
    zerocouponbond.cpp \
    zerocurve.cpp

