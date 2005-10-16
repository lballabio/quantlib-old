
# srcgen.py probably needs to be re-executed if any of these change

SRCGEN_INPUT := calc.xml \
                capfloor.xml \
                config.xml \
                couponvectors.xml \
                c.xml \
                doxygen.xml \
                enums.xml \
                excel.xml \
                guile.xml \
                instruments.xml \
                interpolation.xml \
                objecthandler.xml \
                options.xml \
                processes.xml \
                schedule.xml \
                shortratemodels.xml \
                simpleswap.xml \
                swap.xml \
                termstructures.xml \
                utilities.xml \
                volatilities.xml \
                xibor.xml

SRCGEN_PYS := addinCalc.py \
              addinC.py \
              addinDoxygen.py \
              addinExcel.py \
              addinGuile.py \
              addin.py \
              addinQla.py \
              category.py \
              common.py \
              config.py \
              function.py \
              parameter.py \
              parse.py \
              rule.py \
              srcgen.py \
              utils.py

SRCGEN_STUBS := stub.calc.constructor \
                stub.calc.idlfoot \
                stub.calc.idlfunc \
                stub.calc.idlhead \
                stub.calc.includes \
                stub.calc.map \
                stub.calc.member \
                stub.c.constructor \
                stub.c.includes \
                stub.c.member \
                stub.copyright \
                stub.doxygen.enums \
                stub.doxygen.functions \
                stub.excel.constructor \
                stub.excel.includes \
                stub.excel.member \
                stub.excel.regheader \
                stub.guile.constructor \
                stub.guile.includes \
                stub.guile.initfunc \
                stub.guile.member \
                stub.qla.includes

# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             Makefile.vc \
             srcgen.dsp \
             srcgen.vcproj \
             srcgen_vc8.vcproj \
             $(SRCGEN_INPUT) \
             $(SRCGEN_PYS) \
             $(SRCGEN_STUBS)

# command line arguments
SRCGEN_ARGS = -dq
if BUILD_C
    SRCGEN_ARGS += -c
endif
if BUILD_CALC
    SRCGEN_ARGS += -o
endif
if BUILD_EXCEL_DYNAMIC
    SRCGEN_ARGS += -e
endif
if BUILD_EXCEL_STATIC
    SRCGEN_ARGS += -s
endif
if BUILD_GUILE
    SRCGEN_ARGS += -g
endif

all-local: .time-stamp

.time-stamp: $(SRCGEN_INPUT) $(SRCGEN_PYS) $(SRCGEN_STUBS)
	python srcgen.py $(SRCGEN_ARGS)
	touch .time-stamp

