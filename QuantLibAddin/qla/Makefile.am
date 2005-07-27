
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qla

lib_LTLIBRARIES = libQuantLibAddin.la

EXTRA_DIST = \
    autolink.hpp

include_HEADERS = \
    asianoption.hpp \
    barrieroption.hpp \
    basketoption.hpp \
    capfloor.hpp \
    cliquetoption.hpp \
    config.hpp \
    dividendvanillaoption.hpp \
    fixedcouponbond.hpp \
    forwardvanillaoption.hpp \
    generalutils.hpp \
    instruments.hpp \
    options.hpp \
    processes.hpp \
    qladdin.hpp \
    qladdindefines.hpp \
    shortratemodels.hpp \
    simpleswap.hpp \
    termstructures.hpp \
    typefactory.hpp \
    typeregistry.hpp \
    utilities.hpp \
    vanillaoption.hpp \
    volatilities.hpp \
    xibor.hpp \
    zerocouponbond.hpp \
    zerocurve.hpp

libQuantLibAddin_la_SOURCES = \
    asianoption.cpp \
    barrieroption.cpp \
    basketoption.cpp \
    capfloor.cpp \
    cliquetoption.cpp \
    complextyperegistry.cpp \
    dividendvanillaoption.cpp \
    enumregistry.cpp \
    fixedcouponbond.cpp \
    forwardvanillaoption.cpp \
    generalutils.cpp \
    processes.cpp \
    shortratemodels.cpp \
    simpleswap.cpp \
    termstructures.cpp \
    utilities.cpp \
    vanillaoption.cpp \
    volatilities.cpp \
    xibor.cpp \
    zerocouponbond.cpp \
    zerocurve.cpp

