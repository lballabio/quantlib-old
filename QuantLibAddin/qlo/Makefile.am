
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo

lib_LTLIBRARIES = libQuantLibAddin.la

EXTRA_DIST = \
    auto_link.hpp

include_HEADERS = \
    abcd.hpp \
    accountingengines.hpp \
    alphaform.hpp \
    asianoption.hpp \
    assetswap.hpp \
    barrieroption.hpp \
    baseinstruments.hpp \
    bonds.hpp \
    browniangenerators.hpp \
    capfloor.hpp \
    capletvolstructure.hpp \
    cliquetoption.hpp \
    cmsmarket.hpp \
    cmsmarketcalibration.hpp \
    config.hpp \
    conundrumpricer.hpp \
    correlation.hpp \
    couponvectors.hpp \
    ctsmmcapletcalibration.hpp \
    curvestate.hpp \
    date.hpp \
    dividendvanillaoption.hpp \
    driftcalculators.hpp \
    europeanoption.hpp \
    evolutiondescription.hpp \
    exercise.hpp \
    flowanalysis.hpp \
    forwardrateagreement.hpp \
    forwardvanillaoption.hpp \
    getcovariance.hpp \
    handle.hpp \
    handleimpl.hpp \
    index.hpp \
    interpolation2D.hpp \
    interpolation.hpp \
    leg.hpp \
    marketmodelevolvers.hpp \
    marketmodels.hpp \
    mathf.hpp \
    optimization.hpp \
    options.hpp \
    payoffs.hpp \
    pricingengines.hpp \
    processes.hpp \
    products.hpp \
    qladdindefines.hpp \
    qladdin.hpp \
    quantoforwardvanillaoption.hpp \
    quantovanillaoption.hpp \
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
    stickyratchet.hpp \
    swap.hpp \
    swaption.hpp \
    swaptionvolstructure.hpp \
    symmetricschurdecomposition.hpp \
    termstructures.hpp \
    utilities.hpp \
    vanillaoption.hpp \
    vanillaswap.hpp \
    vcconfig.hpp \
    volatilities.hpp \
    volatility.hpp

libQuantLibAddin_la_SOURCES = \
    abcd.cpp \
    accountingengines.cpp \
    alphaform.cpp \
    asianoption.cpp \
    assetswap.cpp \
    barrieroption.cpp \
    bonds.cpp \
    browniangenerators.cpp \
    capfloor.cpp \
    capletvolstructure.cpp \
    cliquetoption.cpp \
    cmsmarket.cpp \
    cmsmarketcalibration.cpp \
    conundrumpricer.cpp \
    correlation.cpp \
    couponvectors.cpp \
    ctsmmcapletcalibration.cpp \
    curvestate.cpp \
    date.cpp \
    dividendvanillaoption.cpp \
    driftcalculators.cpp \
    europeanoption.cpp \
    evolutiondescription.cpp \
    exercise.cpp \
    flowanalysis.cpp \
    forwardrateagreement.cpp \
    forwardvanillaoption.cpp \
    getcovariance.cpp \
    index.cpp \
    interpolation2D.cpp \
    interpolation.cpp \
    leg.cpp \
    marketmodelevolvers.cpp \
    marketmodels.cpp \
    optimization.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    products.cpp \
    quantoforwardvanillaoption.cpp \
    quantovanillaoption.cpp \
    quotes.cpp \
    randomsequencegenerator.cpp \
    rangeaccrual.cpp \
    ratehelpers.cpp \
    schedule.cpp \
    sequencestatistics.cpp \
    settings.cpp \
    shortratemodels.cpp \
    smilesection.cpp \
    statistics.cpp \
    stickyratchet.cpp \
    swap.cpp \
    swaption.cpp \
    swaptionvolstructure.cpp \
    symmetricschurdecomposition.cpp \
    termstructures.cpp \
    utilities.cpp \
    vanillaoption.cpp \
    vanillaswap.cpp \
    volatilities.cpp \
    volatility.cpp

libQuantLibAddin_la_LIBADD = \
    Conversions/libConversions.la \
    Enumerations/Constructors/libConstructors.la \
    Enumerations/Factories/libFactories.la \
    Enumerations/Register/libRegister.la \
    Serialization/libSerialization.la \
    ValueObjects/libValueObjects.la

