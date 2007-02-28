
# gensrc.py probably needs to be re-executed if any of these change

GENSRC_INPUT := metadata/assetswap.xml \
                metadata/bonds.xml \
                metadata/calendar.xml \
                metadata/capfloor.xml \
                metadata/capletvolstructure.xml \
                metadata/cmsmarket.xml \
                metadata/couponvectors.xml \
                metadata/date.xml \
                metadata/daycounter.xml \
                metadata/enumclasses.xml \
                metadata/enumcurves.xml \
                metadata/enumtypes.xml \
                metadata/exercise.xml \
                metadata/forwardrateagreement.xml \
                metadata/index.xml \
                metadata/instruments.xml \
                metadata/interpolation.xml \
                metadata/marketmodels.xml \
                metadata/mathf.xml \
                metadata/ohfunctions.xml \
                metadata/optimization.xml \
                metadata/options.xml \
                metadata/payoffs.xml \
                metadata/prices.xml \
                metadata/pricingengines.xml \
                metadata/processes.xml \
                metadata/quotes.xml \
                metadata/randomsequencegenerator.xml \
                metadata/ratehelpers.xml \
                metadata/schedule.xml \
                metadata/sequencestatistics.xml \
                metadata/settings.xml \
                metadata/shortratemodels.xml \
                metadata/simplecashflow.xml \
                metadata/smilesection.xml \
                metadata/statistics.xml \
                metadata/swaptionvolstructure.xml \
                metadata/swaption.xml \
                metadata/swap.xml \
                metadata/termstructures.xml \
                metadata/utilities.xml \
                metadata/vanillaswap.xml \
                metadata/volatilities.xml

GENSRC_STUBS := stubs/stub.copyright \
                stubs/stub.excel.includes

# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             qlgensrc_vc8.vcproj \
             qlgensrc.vcproj \
             Makefile.vc \
             $(GENSRC_INPUT) \
             $(GENSRC_STUBS)

# Get the command line arguments to be passed to gensrc.
# In all cases we will specify:
# q - the core QuantLibAddin code
# v - Value Objects
# l - typedefs for Loop functions
# d - Doxygen documentation files
# Depending on the arguments that were passed to configure,
# we may also specify:
# c - C addin
# o - OpenOffice.org Calc addin
# e - Excel addin
# g - Guile addin

GENSRC_ARGS = -dqvl
if BUILD_C
    GENSRC_ARGS += -c
endif
if BUILD_CALC
    GENSRC_ARGS += -o
endif
if BUILD_EXCEL
    GENSRC_ARGS += -e
endif
if BUILD_GUILE
    GENSRC_ARGS += -g
endif

all-local: .time-stamp

.time-stamp: $(GENSRC_INPUT) $(GENSRC_STUBS)
	python ../../gensrc/gensrc.py $(GENSRC_ARGS)
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

