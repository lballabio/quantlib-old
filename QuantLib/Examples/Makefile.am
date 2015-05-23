
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

SUBDIRS = \
	AswZSpreads \
    BasketLosses \
    BermudanSwaption \
	BermudanSwaption2 \
    Bonds \
    CallableBonds \
	CallableBonds2 \
    CDS \
    ConvertibleBonds \
	CreditRiskPlus \
    DiscreteHedging \
    EquityOption \
    FittedBondCurve \
    FRA \
    Gaussian1dModels \
	KahaleSmileSection \
    MarketModels \
	MultitenorVols \
    LatentModel \
    MultidimIntegral \
    Replication \
    Repo \
	SviSmileSection \
    Swap \
	Volaswap

SUBDIR_CHECKS = $(SUBDIRS:%=%.check)

EXTRA_DIST = README.txt

%.check:
	$(MAKE) -C $* check-examples

.PHONY: examples check-examples $(SUBDIRS)

examples: $(SUBDIRS)

check-examples: $(SUBDIR_CHECKS)

$(SUBDIRS):
	$(MAKE) -C $@ examples
