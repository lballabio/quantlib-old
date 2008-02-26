
GENSRC_DIR = @gensrc_dir@

# gensrc.py probably needs to be re-executed if any of these change

GENSRC_METADATA :=  metadata/Addins/calc.xml \
                    metadata/Addins/cpp.xml \
                    metadata/Addins/c.xml \
                    metadata/Addins/doxygen.xml \
                    metadata/Addins/enumerations.xml \
                    metadata/Addins/excel.xml \
                    metadata/Addins/guile.xml \
                    metadata/Addins/loop.xml \
                    metadata/Addins/serialization.xml \
                    metadata/Addins/valueobjects.xml \
                    metadata/Enumerations/enumeratedclasses.xml \
                    metadata/Enumerations/enumeratedpairs.xml \
                    metadata/Enumerations/enumeratedtypes.xml \
                    metadata/Functions/abcd.xml \
                    metadata/Functions/accountingengines.xml \
                    metadata/Functions/alphaform.xml \
                    metadata/Functions/assetswap.xml \
                    metadata/Functions/bonds.xml \
                    metadata/Functions/browniangenerators.xml \
                    metadata/Functions/calendar.xml \
                    metadata/Functions/capfloor.xml \
                    metadata/Functions/capletvolstructure.xml \
                    metadata/Functions/cmsmarketcalibration.xml \
                    metadata/Functions/cmsmarket.xml \
                    metadata/Functions/correlation.xml \
                    metadata/Functions/couponvectors.xml \
                    metadata/Functions/ctsmmcapletcalibration.xml \
                    metadata/Functions/curvestate.xml \
                    metadata/Functions/date.xml \
                    metadata/Functions/daycounter.xml \
                    metadata/Functions/driftcalculators.xml \
                    metadata/Functions/evolutiondescription.xml \
                    metadata/Functions/exercise.xml \
                    metadata/Functions/forwardrateagreement.xml \
                    metadata/Functions/handles.xml \
                    metadata/Functions/index.xml \
                    metadata/Functions/instruments.xml \
                    metadata/Functions/interpolation.xml \
                    metadata/Functions/leg.xml \
                    metadata/Functions/marketmodelevolvers.xml \
                    metadata/Functions/marketmodels.xml \
                    metadata/Functions/mathf.xml \
                    metadata/Functions/optimization.xml \
                    metadata/Functions/options.xml \
                    metadata/Functions/payoffs.xml \
                    metadata/Functions/piecewiseyieldcurve.xml \
                    metadata/Functions/prices.xml \
                    metadata/Functions/pricingengines.xml \
                    metadata/Functions/processes.xml \
                    metadata/Functions/products.xml \
                    metadata/Functions/quotes.xml \
                    metadata/Functions/randomsequencegenerator.xml \
                    metadata/Functions/rangeaccrual.xml \
                    metadata/Functions/ratehelpers.xml \
                    metadata/Functions/schedule.xml \
                    metadata/Functions/sequencestatistics.xml \
                    metadata/Functions/settings.xml \
                    metadata/Functions/shortratemodels.xml \
                    metadata/Functions/simplecashflow.xml \
                    metadata/Functions/smilesection.xml \
                    metadata/Functions/statistics.xml \
                    metadata/Functions/swaptionvolstructure.xml \
                    metadata/Functions/swaption.xml \
                    metadata/Functions/swap.xml \
                    metadata/Functions/termstructures.xml \
                    metadata/Functions/timeseries.xml \
                    metadata/Functions/utilities.xml \
                    metadata/Functions/vanillaswap.xml \
                    metadata/Functions/volatilities.xml \
                    metadata/Functions/volatility.xml \
                    metadata/Rules/calc.xml \
                    metadata/Rules/cpp.xml \
                    metadata/Rules/c.xml \
                    metadata/Rules/doxygen.xml \
                    metadata/Rules/excel.xml \
                    metadata/Rules/guile.xml \
                    metadata/Rules/loop.xml \
                    metadata/Rules/serialization.xml \
                    metadata/Rules/valueobjects.xml \
                    metadata/Types/supertypes.xml \
                    metadata/Types/types.xml

GENSRC_STUBS :=     stubs/stub.copyright \
                    stubs/stub.cpp.body \
                    stubs/stub.doxygen.enums \
                    stubs/stub.enum.classes \
                    stubs/stub.enum.pairs \
                    stubs/stub.enum.types \
                    stubs/stub.excel.includes \
                    stubs/stub.serialization.includes

GENSRC_CODE :=      code/codedict.py

GENSRC_CONF :=      config/config.xml

GENSRC_INPUTS = $(GENSRC_METADATA) $(GENSRC_STUBS) $(GENSRC_CODE) $(GENSRC_CONF)

# This should prevent running from a distributed tarball

EXTRA_DIST = .time-stamp \
             qlgensrc_vc8.vcproj \
             qlgensrc_vc7.vcproj \
             Makefile.vc \
             $(GENSRC_INPUTS)

# Get the command line arguments to be passed to gensrc.
# In all cases we will specify:
# v - Value Objects
# s - Serialization
# l - Loop functions
# e - Enumerations
# d - Doxygen documentation files
# Depending on the arguments that were passed to configure,
# we may also specify:
# x - Excel addin
# o - OpenOffice.org Calc addin
# p - C++ addin
# [g - Guile addin]
# [c - C addin]

GENSRC_ARGS = -vsled
if BUILD_EXCEL
    GENSRC_ARGS += -x
endif
if BUILD_CALC
    GENSRC_ARGS += -o
endif
if BUILD_CPP
    GENSRC_ARGS += -p
endif
#if BUILD_GUILE
#    GENSRC_ARGS += -g
#endif
#if BUILD_C
#    GENSRC_ARGS += -c
#endif

all-local: .time-stamp

.time-stamp: $(GENSRC_INPUTS)
	python $(GENSRC_DIR)/gensrc.py $(GENSRC_ARGS)
	touch .time-stamp

dist-hook:
	mkdir -p $(distdir)/code
	mkdir -p $(distdir)/config
	mkdir -p $(distdir)/metadata
	mkdir -p $(distdir)/metadata/Addins
	mkdir -p $(distdir)/metadata/Enumerations
	mkdir -p $(distdir)/metadata/Functions
	mkdir -p $(distdir)/metadata/Rules
	mkdir -p $(distdir)/metadata/Types
	mkdir -p $(distdir)/stubs
	cp -p $(srcdir)/code/*.py $(distdir)/code
	cp -p $(srcdir)/config/*.xml $(distdir)/config
	cp -p $(srcdir)/metadata/Addins/*.xml $(distdir)/metadata/Addins
	cp -p $(srcdir)/metadata/Enumerations/*.xml $(distdir)/metadata/Enumerations
	cp -p $(srcdir)/metadata/Functions/*.xml $(distdir)/metadata/Functions
	cp -p $(srcdir)/metadata/Rules/*.xml $(distdir)/metadata/Rules
	cp -p $(srcdir)/metadata/Types/*.xml $(distdir)/metadata/Types
	cp -p $(srcdir)/stubs/stub.* $(distdir)/stubs
	touch .time-stamp

clean:
	rm -f .time-stamp

