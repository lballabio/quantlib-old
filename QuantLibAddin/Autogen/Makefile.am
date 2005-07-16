
# autogen.py probably needs to be re-executed if any of these change
AUTOGEN_INPUT := capfloor.xml enums.xml instruments.xml options.xml processes.xml \
                 qlatest.xml shortratemodels.xml termstructures.xml utilities.xml \
                 volatilities.xml simpleswap.xml xibor.xml
AUTOGEN_PYS := autogen.py c.py calc.py common.py doxygen.py excel.py \
               guile.py params.py parse.py qla.py utils.py
AUTOGEN_STUBS := stub.Qla.includes stub.C.body stub.C.includes \
                 stub.Calc.body stub.Calc.idlfoot stub.Calc.idlfunc \
                 stub.Calc.idlhead stub.Calc.includes stub.Calc.map \
                 stub.Excel.body stub.Excel.includes stub.Excel.regheader \
                 stub.Doxygen.enums stub.copyright

# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             Makefile.msdev \
             Autogen.dsp \
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

