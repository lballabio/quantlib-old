
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
    capfloor.hpp \
    cliquetoption.hpp \
    dividendvanillaoption.hpp \
    fixedcouponbond.hpp \
    forwardvanillaoption.hpp \
    generalutils.hpp \
    instruments.hpp \
    options.hpp \
    optionutils.hpp \
    processes.hpp \
    qladdin.hpp \
    qladdindefines.hpp \
    shortratemodels.hpp \
    termstructures.hpp \
    utilities.hpp \
    vanillaoption.hpp \
    volatilities.hpp \
    zerocouponbond.hpp \
    zerocurve.hpp

lib_LTLIBRARIES = libQuantLibAddin.la

libQuantLibAddin_la_SOURCES = \
    asianoption.cpp \
    barrieroption.cpp \
    basketoption.cpp \
    capfloor.cpp \
    cliquetoption.cpp \
    dividendvanillaoption.cpp \
    fixedcouponbond.cpp \
    forwardvanillaoption.cpp \
    generalutils.cpp \
    optionutils.cpp \
    processes.cpp \
    shortratemodels.cpp \
    termstructures.cpp \
    utilities.cpp \
    vanillaoption.cpp \
    volatilities.cpp \
    zerocouponbond.cpp \
    zerocurve.cpp

