
# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp

# autogen.py probably needs to be re-executed if any of these change
AUTOGEN_INPUT := utilities.xml options.xml
AUTOGEN_PYS := autogen.py c.py calc.py common.py excel.py \
               parse.py qla.py utils.py
AUTOGEN_STUBS := stub.C.body stub.C.includes \
                 stub.Calc.body stub.Calc.idlfoot stub.Calc.idlfunc \
                 stub.Calc.idlhead stub.Calc.includes stub.Calc.map \
                 stub.Excel.body stub.Excel.includes \
                 stub.Excel.regfooter stub.Excel.regheader \
                 stub.qla.constructor stub.qla.conversion \
                 stub.qla.function stub.qla.header stub.qla.includes

all-local: .time-stamp

.time-stamp: $(AUTOGEN_INPUT) $(AUTOGEN_PYS) $(AUTOGEN_STUBS)
	python autogen.py
	touch .time-stamp

