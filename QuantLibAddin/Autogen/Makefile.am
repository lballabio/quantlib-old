
# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             Makefile.msdev \
             Autogen.dsp \
             Autogen_vc8.vcproj

# autogen.py probably needs to be re-executed if any of these change
AUTOGEN_INPUT := capfloor.xml instruments.xml options.xml processes.xml qlatest.xml \
                 shortratemodels.xml termstructures.xml utilities.xml volatilities.xml \
                 simpleswap.xml xibor.xml
AUTOGEN_PYS := autogen.py c.py calc.py common.py excel.py \
               params.py parse.py utils.py doxygen.py
AUTOGEN_STUBS := stub.C.body stub.C.includes \
                 stub.Calc.body stub.Calc.idlfoot stub.Calc.idlfunc \
                 stub.Calc.idlhead stub.Calc.includes stub.Calc.map \
                 stub.Excel.body stub.Excel.includes stub.Excel.regheader \
                 stub.copyright

# command line arguments
AUTOGEN_ARGS = -d
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

