
# gensrc.py probably needs to be re-executed if any of these change

GENSRC_METADATA :=  metadata/Addins/calc.xml \
                    metadata/Addins/c.xml \
                    metadata/Addins/doxygen.xml \
                    metadata/Addins/enumerations.xml \
                    metadata/Addins/excel.xml \
                    metadata/Addins/guile.xml \
                    metadata/Addins/loop.xml \
                    metadata/Addins/valueobjects.xml \
                    metadata/Enumerations/enumeratedclasses.xml \
                    metadata/Enumerations/enumeratedcurves.xml \
                    metadata/Enumerations/enumeratedtypes.xml \
                    metadata/Functions/assetswap.xml \
                    metadata/Functions/bonds.xml \
                    metadata/Functions/calendar.xml \
                    metadata/Functions/capfloor.xml \
                    metadata/Functions/capletvolstructure.xml \
                    metadata/Functions/cmsmarket.xml \
                    metadata/Functions/couponvectors.xml \
                    metadata/Functions/date.xml \
                    metadata/Functions/daycounter.xml \
                    metadata/Functions/exercise.xml \
                    metadata/Functions/forwardrateagreement.xml \
                    metadata/Functions/index.xml \
                    metadata/Functions/instruments.xml \
                    metadata/Functions/interpolation.xml \
                    metadata/Functions/marketmodels.xml \
                    metadata/Functions/mathf.xml \
                    metadata/Functions/ohfunctions.xml \
                    metadata/Functions/optimization.xml \
                    metadata/Functions/options.xml \
                    metadata/Functions/payoffs.xml \
                    metadata/Functions/prices.xml \
                    metadata/Functions/pricingengines.xml \
                    metadata/Functions/processes.xml \
                    metadata/Functions/quotes.xml \
                    metadata/Functions/randomsequencegenerator.xml \
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
                    metadata/Functions/utilities.xml \
                    metadata/Functions/vanillaswap.xml \
                    metadata/Functions/volatilities.xml \
                    metadata/Rules/calc.xml \
                    metadata/Rules/c.xml \
                    metadata/Rules/doxygen.xml \
                    metadata/Rules/excel.xml \
                    metadata/Rules/guile.xml \
                    metadata/Rules/loop.xml \
                    metadata/Rules/valueobjects.xml \
                    metadata/Types/types.xml

GENSRC_STUBS :=     stubs/stub.copyright \
                    stubs/stub.doxygen.enums \
                    stubs/stub.enum.classes \
                    stubs/stub.enum.types \
                    stubs/stub.excel.includes

GENSRC_CODE :=      code/codedict.py

GENSRC_CONF :=      config/config.xml

GENSRC_INPUTS = $(GENSRC_METADATA) $(GENSRC_STUBS) $(GENSRC_CODE) $(GENSRC_CONF)

# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             qlgensrc_vc8.vcproj \
             qlgensrc.vcproj \
             Makefile.vc \
             $(GENSRC_INPUTS)

# Get the command line arguments to be passed to gensrc.
# In all cases we will specify:
# v - Value Objects
# l - Loop functions
# e - Enumerations
# d - Doxygen documentation files
# Depending on the arguments that were passed to configure,
# we may also specify:
# x - Excel addin
# o - OpenOffice.org Calc addin
# g - Guile addin
# c - C addin

GENSRC_ARGS = -vled
if BUILD_EXCEL
    GENSRC_ARGS += -x
endif
#if BUILD_CALC
#    GENSRC_ARGS += -o
#endif
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
	mkdir -p $(distdir)/config $(distdir)/metadata $(distdir)/scripts $(distdir)/stubs
	cp -p $(srcdir)/config/*.xml $(distdir)/config
	cp -p $(srcdir)/metadata/*.xml $(distdir)/metadata
	cp -p $(srcdir)/scripts/*.py $(distdir)/scripts
	cp -p $(srcdir)/stubs/stub.* $(distdir)/stubs
	touch .time-stamp

clean:
	rm -f .time-stamp

