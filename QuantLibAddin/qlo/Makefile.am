
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo

lib_LTLIBRARIES = libQuantLibAddin.la

EXTRA_DIST = \
    auto_link.hpp

include_HEADERS = \
    asianoption.hpp \
    assetswap.hpp \
    barrieroption.hpp \
    baseinstruments.hpp \
    bonds.hpp \
    capfloor.hpp \
    capletvolstructure.hpp \
    cliquetoption.hpp \
    cmsmarket.hpp \
    config.hpp \
    conundrumpricer.hpp \
    conundrumpricer.hpp \
    couponvectors.hpp \
    date.hpp \
    dividendvanillaoption.hpp \
    europeanoption.hpp \
    exercise.hpp \
    flowanalysis.hpp \
    forwardrateagreement.hpp \
    forwardvanillaoption.hpp \
    getcovariance.hpp \
    handle.hpp \
    index.hpp \
    interpolation.hpp \
    interpolation2D.hpp \
    leg.hpp \
    marketmodels.hpp \
    mathf.hpp \
    optimization.hpp \
    options.hpp \
    payoffs.hpp \
    pricingengines.hpp \
    processes.hpp \
    qladdin.hpp \
    qladdindefines.hpp \
    quantoforwardvanillaoption.hpp \
    quantovanillaoption.hpp \
    quotes.hpp \
    randomsequencegenerator.hpp \
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
    volatilities.hpp

libQuantLibAddin_la_SOURCES = \
    asianoption.cpp \
    assetswap.cpp \
    barrieroption.cpp \
    bonds.cpp \
    capfloor.cpp \
    capletvolstructure.cpp \
    cliquetoption.cpp \
    cmsmarket.cpp \
    conundrumpricer.cpp \
    couponvectors.cpp \
    date.cpp \
    dividendvanillaoption.cpp \
    europeanoption.cpp \
    exercise.cpp \
    flowanalysis.cpp \
    forwardrateagreement.cpp \
    forwardvanillaoption.cpp \
    getcovariance.cpp \
    index.cpp \
    interpolation.cpp \
    interpolation2D.cpp \
    leg.hpp \
    marketmodels.cpp \
    optimization.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    quantoforwardvanillaoption.cpp \
    quantovanillaoption.cpp \
    quotes.cpp \
    randomsequencegenerator.cpp \
    rangeaccrual.cpp \
    ratehelpers.cpp \
    schedule.cpp \
    sequencestatistics.cpp \
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
    volatilities.cpp

libQuantLibAddin_la_LIBADD = \
    Conversions/libConversions.la \
    Enumerations/Constructors/libConstructors.la \
    Enumerations/Factories/libFactories.la \
    Enumerations/Register/libRegister.la \
    ValueObjects/libValueObjects.la

