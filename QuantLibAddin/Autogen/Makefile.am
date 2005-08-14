
# autogen.py probably needs to be re-executed if any of these change
AUTOGEN_INPUT := capfloor.xml enums.xml instruments.xml options.xml processes.xml \
                 shortratemodels.xml termstructures.xml utilities.xml \
                 volatilities.xml simpleswap.xml xibor.xml
AUTOGEN_PYS := autogen.py c.py calc.py common.py doxygen.py excel.py \
               guile.py params.py parse.py qla.py utils.py
AUTOGEN_STUBS := stub.qla.includes stub.c.constructor stub.c.member stub.c.includes \
                 stub.calc.constructor stub.calc.member \
                 stub.calc.idlfoot stub.calc.idlfunc \
                 stub.calc.idlhead stub.calc.includes stub.calc.map \
                 stub.excel.constructor stub.excel.member \
                 stub.excel.includes stub.excel.regheader \
                 stub.guile.constructor stub.guile.member \
                 stub.guile.initfunc stub.guile.includes \
                 stub.doxygen.enums stub.doxygen.functions stub.copyright
# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             Makefile.vc \
             Autogen.dsp \
             Autogen.vcproj \
             Autogen_vc8.vcproj \
             $(AUTOGEN_INPUT) \
             $(AUTOGEN_PYS) \
             $(AUTOGEN_STUBS)

# command line arguments
AUTOGEN_ARGS = -dq
if BUILD_C
    AUTOGEN_ARGS += -c
endif
if BUILD_CALC
    AUTOGEN_ARGS += -o
endif
if BUILD_EXCEL
    AUTOGEN_ARGS += -e
endif
if BUILD_GUILE
    AUTOGEN_ARGS += -g
endif

all-local: .time-stamp

.time-stamp: $(AUTOGEN_INPUT) $(AUTOGEN_PYS) $(AUTOGEN_STUBS)
	python autogen.py $(AUTOGEN_ARGS)
	touch .time-stamp

