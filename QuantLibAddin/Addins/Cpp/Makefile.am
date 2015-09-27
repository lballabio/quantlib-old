 
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo/Addins/Cpp

EXTRA_DIST = \
    AddinCpp_vc8.vcproj \
    AddinCpp_vc9.vcproj \
    AddinCpp_vc10.vcxproj \
    AddinCpp_vc10.vcxproj.filters \
    AddinCpp_vc11.vcxproj \
    AddinCpp_vc11.vcxproj.filters \
    AddinCpp_vc12.vcxproj \
    AddinCpp_vc12.vcxproj.filters

if BUILD_CPP
lib_LTLIBRARIES = libQuantLibAddinCpp.la
else
EXTRA_LTLIBRARIES = libQuantLibAddinCpp.la
endif

if BUILD_CPP
include_HEADERS = \
    abcd.hpp \
    accountingengines.hpp \
    addincppdefines.hpp \
    addincpp.hpp \
    alphaform.hpp \
    assetswap.hpp \
    basketlossmodels.hpp \
    bonds.hpp \
    browniangenerators.hpp \
    btp.hpp \
    capfloor.hpp \
    capletvolstructure.hpp \
    cmsmarket.hpp \
    correlation.hpp \
    couponvectors.hpp \
    credit.hpp \
    ctsmmcapletcalibration.hpp \
    curvestate.hpp \
    defaulttermstructures.hpp \
    evolutiondescription.hpp \
    exercise.hpp \
    forwardrateagreement.hpp \
    garbagecollection.hpp \
    handles.hpp \
    index.hpp \
    init.hpp \
    instruments.hpp \
    interpolation.hpp \
    leg.hpp \
    logging.hpp \
    loop.hpp \
    marketmodelevolvers.hpp \
    marketmodels.hpp \
    ohutils.hpp \
    optimization.hpp \
    options.hpp \
    overnightindexedswap.hpp \
    payoffs.hpp \
    piecewiseyieldcurve.hpp \
    pricingengines.hpp \
    processes.hpp \
    products.hpp \
    quotes.hpp \
    randomsequencegenerator.hpp \
    rangeaccrual.hpp \
    ratehelpers.hpp \
    schedule.hpp \
    sequencestatistics.hpp \
    serialization.hpp \
    settings.hpp \
    shortratemodels.hpp \
    smilesection.hpp \
    statistics.hpp \
    swap.hpp \
    swaption.hpp \
    swaptionvolstructure.hpp \
    termstructures.hpp \
    timeseries.hpp \
    utilities.hpp \
    valueobjects.hpp \
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
    basketlossmodels.cpp \
    bonds.cpp \
    browniangenerators.cpp \
    btp.cpp \
    capfloor.cpp \
    capletvolstructure.cpp \
    cmsmarket.cpp \
    correlation.cpp \
    couponvectors.cpp \
    credit.cpp \
    ctsmmcapletcalibration.cpp \
    curvestate.cpp \
    defaulttermstructures.cpp \
    evolutiondescription.cpp \
    exercise.cpp \
    forwardrateagreement.cpp \
    garbagecollection.cpp \
    handles.cpp \
    index.cpp \
    init.cpp \
    instruments.cpp \
    interpolation.cpp \
    leg.cpp \
    logging.cpp \
    marketmodelevolvers.cpp \
    marketmodels.cpp \
    ohutils.cpp \
    optimization.cpp \
    options.cpp \
    overnightindexedswap.cpp \
    payoffs.cpp \
    piecewiseyieldcurve.cpp \
    pricingengines.cpp \
    processes.cpp \
    products.cpp \
    quotes.cpp \
    randomsequencegenerator.cpp \
    rangeaccrual.cpp \
    ratehelpers.cpp \
    schedule.cpp \
    sequencestatistics.cpp \
    serialization.cpp \
    settings.cpp \
    shortratemodels.cpp \
    smilesection.cpp \
    statistics.cpp \
    swap.cpp \
    swaption.cpp \
    swaptionvolstructure.cpp \
    termstructures.cpp \
    timeseries.cpp \
    utilities.cpp \
    valueobjects.cpp \
    vanillaswap.cpp \
    volatilities.cpp \
    volatility.cpp
endif

libQuantLibAddinCpp_la_LIBADD = \
    ../../qlo/libQuantLibAddin.la

libQuantLibAddinCpp_la_LDFLAGS = \
-lQuantLib -lObjectHandler -lboost_filesystem -lboost_serialization -lboost_system -lboost_regex

