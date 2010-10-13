
GENSRC_DIR = @gensrc_dir@
OH_DIR = @oh_dir@

# gensrc.py probably needs to be re-executed if any of these change

GENSRC_METADATA :=  metadata/addins/calc.xml \
                    metadata/addins/cpp.xml \
                    metadata/addins/c.xml \
                    metadata/addins/doxygen.xml \
                    metadata/addins/enumerations.xml \
                    metadata/addins/excel.xml \
                    metadata/addins/guile.xml \
                    metadata/addins/loop.xml \
                    metadata/addins/serialization.xml \
                    metadata/addins/valueobjects.xml \
                    metadata/enumerations/enumeratedclasses.xml \
                    metadata/enumerations/enumeratedpairs.xml \
                    metadata/enumerations/enumeratedtypes.xml \
                    metadata/functions/abcd.xml \
                    metadata/functions/accountingengines.xml \
                    metadata/functions/alphaform.xml \
                    metadata/functions/assetswap.xml \
                    metadata/functions/bonds.xml \
                    metadata/functions/browniangenerators.xml \
                    metadata/functions/calendar.xml \
                    metadata/functions/capfloor.xml \
                    metadata/functions/capletvolstructure.xml \
                    metadata/functions/cmsmarketcalibration.xml \
                    metadata/functions/cmsmarket.xml \
                    metadata/functions/correlation.xml \
                    metadata/functions/couponvectors.xml \
                    metadata/functions/credit.xml \
                    metadata/functions/ctsmmcapletcalibration.xml \
                    metadata/functions/curvestate.xml \
                    metadata/functions/date.xml \
                    metadata/functions/daycounter.xml \
                    metadata/functions/driftcalculators.xml \
                    metadata/functions/evolutiondescription.xml \
                    metadata/functions/exercise.xml \
                    metadata/functions/forwardrateagreement.xml \
                    metadata/functions/handles.xml \
                    metadata/functions/index.xml \
                    metadata/functions/instruments.xml \
                    metadata/functions/interpolation.xml \
                    metadata/functions/leg.xml \
                    metadata/functions/marketmodelevolvers.xml \
                    metadata/functions/marketmodels.xml \
                    metadata/functions/mathf.xml \
                    metadata/functions/optimization.xml \
                    metadata/functions/options.xml \
                    metadata/functions/payoffs.xml \
                    metadata/functions/piecewiseyieldcurve.xml \
                    metadata/functions/prices.xml \
                    metadata/functions/pricingengines.xml \
                    metadata/functions/processes.xml \
                    metadata/functions/products.xml \
                    metadata/functions/quotes.xml \
                    metadata/functions/randomsequencegenerator.xml \
                    metadata/functions/rangeaccrual.xml \
                    metadata/functions/ratehelpers.xml \
                    metadata/functions/schedule.xml \
                    metadata/functions/sequencestatistics.xml \
                    metadata/functions/settings.xml \
                    metadata/functions/shortratemodels.xml \
                    metadata/functions/simplecashflow.xml \
                    metadata/functions/smilesection.xml \
                    metadata/functions/statistics.xml \
                    metadata/functions/swaptionvolstructure.xml \
                    metadata/functions/swaption.xml \
                    metadata/functions/swap.xml \
                    metadata/functions/termstructures.xml \
                    metadata/functions/timeseries.xml \
                    metadata/functions/utilities.xml \
                    metadata/functions/vanillaswap.xml \
                    metadata/functions/volatilities.xml \
                    metadata/functions/volatility.xml \
                    metadata/rules/calc.xml \
                    metadata/rules/cpp.xml \
                    metadata/rules/c.xml \
                    metadata/rules/doxygen.xml \
                    metadata/rules/excel.xml \
                    metadata/rules/guile.xml \
                    metadata/rules/loop.xml \
                    metadata/rules/serialization.xml \
                    metadata/rules/valueobjects.xml \
                    metadata/types/supertypes.xml \
                    metadata/types/types.xml

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
# e - enumerations
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
	python $(GENSRC_DIR)/gensrc.py $(GENSRC_ARGS) --oh_dir=$(OH_DIR)
	touch .time-stamp

dist-hook:
	mkdir -p $(distdir)/code
	mkdir -p $(distdir)/config
	mkdir -p $(distdir)/metadata
	mkdir -p $(distdir)/metadata/addins
	mkdir -p $(distdir)/metadata/enumerations
	mkdir -p $(distdir)/metadata/functions
	mkdir -p $(distdir)/metadata/rules
	mkdir -p $(distdir)/metadata/types
	mkdir -p $(distdir)/stubs
	cp -p $(srcdir)/code/*.py $(distdir)/code
	cp -p $(srcdir)/config/*.xml $(distdir)/config
	cp -p $(srcdir)/metadata/addins/*.xml $(distdir)/metadata/addins
	cp -p $(srcdir)/metadata/enumerations/*.xml $(distdir)/metadata/enumerations
	cp -p $(srcdir)/metadata/functions/*.xml $(distdir)/metadata/functions
	cp -p $(srcdir)/metadata/rules/*.xml $(distdir)/metadata/rules
	cp -p $(srcdir)/metadata/types/*.xml $(distdir)/metadata/types
	cp -p $(srcdir)/stubs/stub.* $(distdir)/stubs
	touch .time-stamp

clean:
	rm -f .time-stamp

