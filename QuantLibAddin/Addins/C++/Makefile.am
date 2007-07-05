
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo/Addins/C++

if BUILD_CPP
noinst_LTLIBRARIES = libQuantLibAddinCpp.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinCpp.la
endif

if BUILD_CPP
noinst_HEADERS = \
    abcd.hpp \
    accountingengines.hpp \
    addincpp.hpp \
    alphaform.hpp \
    assetswap.hpp \
    bonds.hpp \
    browniangenerators.hpp \
    capfloor.hpp \
    capletvolstructure.hpp \
    cmsmarket.hpp \
    correlation.hpp \
    couponvectors.hpp \
    ctsmmcapletcalibration.hpp \
    curvestate.hpp \
    evolutiondescription.hpp \
    exercise.hpp \
    forwardrateagreement.hpp \
    index.hpp \
    init.hpp \
    instruments.hpp \
    interpolation.hpp \
    leg.hpp \
    marketmodelevolvers.hpp \
    marketmodels.hpp \
    ohfunctions.hpp \
    optimization.hpp \
    options.hpp \
    payoffs.hpp \
    pricingengines.hpp \
    processes.hpp \
    products.hpp \
    quotes.hpp \
    randomsequencegenerator.hpp \
    rangeaccrual.hpp \
    ratehelpers.hpp \
    schedule.hpp \
    sequencestatistics.hpp \
    settings.hpp \
    shortratemodels.hpp \
    simplecashflow.hpp \
    smilesection.hpp \
    statistics.hpp \
    swap.hpp \
    swaption.hpp \
    swaptionvolstructure.hpp \
    termstructures.hpp \
    utilities.hpp \
    vanillaswap.hpp \
    volatilities.hpp \
    volatility.hpp
endif

if BUILD_CPP
libQuantLibAddinCpp_la_SOURCES = \
    abcd.cpp \
    accountingengines.cpp \
    alphaform.cpp \
    assetswap.cpp \
    bonds.cpp \
    browniangenerators.cpp \
    capfloor.cpp \
    capletvolstructure.cpp \
    cmsmarket.cpp \
    correlation.cpp \
    couponvectors.cpp \
    ctsmmcapletcalibration.cpp \
    curvestate.cpp \
    evolutiondescription.cpp \
    exercise.cpp \
    forwardrateagreement.cpp \
    index.cpp \
    init.cpp \
    instruments.cpp \
    interpolation.cpp \
    leg.cpp \
    marketmodelevolvers.cpp \
    marketmodels.cpp \
    ohfunctions.cpp \
    optimization.cpp \
    options.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    products.cpp \
    quotes.cpp \
    randomsequencegenerator.cpp \
    rangeaccrual.cpp \
    ratehelpers.cpp \
    schedule.cpp \
    sequencestatistics.cpp \
    settings.cpp \
    shortratemodels.cpp \
    simplecashflow.cpp \
    smilesection.cpp \
    statistics.cpp \
    swap.cpp \
    swaption.cpp \
    swaptionvolstructure.cpp \
    termstructures.cpp \
    utilities.cpp \
    vanillaswap.cpp \
    volatilities.cpp \
    volatility.cpp
endif

#libQuantLibAddinCpp_la_LDFLAGS = \
#    -lQuantLib -lObjectHandler

libQuantLibAddinCpp_la_LIBADD = \
    Serialization/libSerialization.la \
    ../../qlo/libQuantLibAddin.la

