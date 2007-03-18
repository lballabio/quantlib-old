
AM_CPPFLAGS = -I${top_srcdir}
AM_LDFLAGS = -release $(PACKAGE_VERSION)
includedir = $(prefix)/include/qlo

lib_LTLIBRARIES = libQuantLibAddin.la
#libQuantLibAddin_la_LDFLAGS = -release $(PACKAGE_VERSION)

EXTRA_DIST = \
    auto_link.hpp

include_HEADERS = \
    analysis.hpp \
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
    couponvectors.hpp \
    date.hpp \
    dividendvanillaoption.hpp \
    enumclassctors.hpp \
    europeanoption.hpp \
    exercise.hpp \
    forwardrateagreement.hpp \
    forwardvanillaoption.hpp \
    getcovariance.hpp \
    handle.hpp \
    index.hpp \
    interpolation2D.hpp \
    interpolation.hpp \
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
    typefactory.hpp \
    typeregistry.hpp \
    utilities.hpp \
    vanillaoption.hpp \
    vanillaswap.hpp \
    vcconfig.hpp \
    volatilities.hpp

libQuantLibAddin_la_SOURCES = \
    analysis.cpp \
    asianoption.cpp \
    assetswap.cpp \
    barrieroption.cpp \
    bonds.cpp \
    calendarfactory.cpp \
    capfloor.cpp \
    capletvolstructure.cpp \
    cliquetoption.cpp \
    cmsmarket.cpp \
    couponvectors.cpp \
    date.cpp \
    dividendvanillaoption.cpp \
    enumclassctors.cpp \
    enumclassregistry.cpp \
    enumtyperegistry.cpp \
    europeanoption.cpp \
    exercise.cpp \
    forwardrateagreement.cpp \
    forwardvanillaoption.cpp \
    getcovariance.cpp \
    index.cpp \
    interpolation2D.cpp \
    interpolation.cpp \
    marketmodels.cpp \
    optimization.cpp \
    payoffs.cpp \
    pricingengines.cpp \
    processes.cpp \
    quantoforwardvanillaoption.cpp \
    quantovanillaoption.cpp \
    quotes.cpp \
    randomsequencegenerator.cpp \
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
    ValueObjects/libValueObjects.la

