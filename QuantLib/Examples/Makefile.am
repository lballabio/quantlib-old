
AM_CPPFLAGS = -I${top_srcdir} -I${top_builddir}

SUBDIRS = \
    BermudanSwaption \
    Bonds \
    CallableBonds \
	CallableBonds2 \
    CDS \
	CmsSwaption \
    ConvertibleBonds \
	CreditRiskPlus \
    DiscreteHedging \
    EquityOption \
    FittedBondCurve \
    FRA \
	KahaleSmileSection \
    MarketModels \
	MultitenorVols \
	NonstandardSwaption \
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
