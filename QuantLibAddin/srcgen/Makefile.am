
# srcgen.py probably needs to be re-executed if any of these change

SRCGEN_INPUT := AddinCalc.xml \
                AddinC.xml \
                AddinDoxygen.xml \
                AddinExcel.xml \
                AddinGuile.xml \
                AddinQla.xml \
                capfloor.xml \
                Config.xml \
                couponvectors.xml \
                enumerations.xml \
                instruments.xml \
                interpolation.xml \
                ohfunctions.xml \
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

SRCGEN_PYS :=   AddinCalc.py \
                AddinC.py \
                AddinDoxygen.py \
                AddinExcel.py \
                AddinGuile.py \
                Addin.py \
                AddinQla.py \
                Buffer.py \
                Category.py \
                common.py \
                Config.py \
                Enumeration.py \
                Factory.py \
                Function.py \
                Log.py \
                OutputFile.py \
                Parameter.py \
                Rule.py \
                Serializable.py \
                Singleton.py \
                srcgen.py \
                XmlReader.py

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
                stub.doxygen.categories \
                stub.doxygen.enums \
                stub.doxygen.header \
                stub.excel.constructor \
                stub.excel.includes \
                stub.excel.member \
                stub.excel.regfooter \
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
if BUILD_EXCEL
    SRCGEN_ARGS += -e
endif
if BUILD_GUILE
    SRCGEN_ARGS += -g
endif

all-local: .time-stamp

.time-stamp: $(SRCGEN_INPUT) $(SRCGEN_PYS) $(SRCGEN_STUBS)
	python srcgen.py $(SRCGEN_ARGS)
	touch .time-stamp

