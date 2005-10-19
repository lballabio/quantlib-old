
SUBDIRS = \
    srcgen \
    qla \
    Addins/C \
    Addins/Calc \
    Addins/ExcelDynamic \
    Addins/ExcelStatic \
    Addins/Guile \
    Clients/C \
    Clients/C++ \
    Clients/Calc \
    Clients/Excel \
    Clients/Guile \
    Docs

EXTRA_DIST = \
    Announce.txt \
    Authors.txt \
    autogen.sh \
    configure.ac \
    Contributors.txt \
    LICENSE.TXT \
    NEWS.txt \
    QuantLibAddin.dsp \
    QuantLibAddin.dsw \
    QuantLibAddin.nsi \
    QuantLibAddin.sln \
    QuantLibAddin.vcproj \
    QuantLibAddin_vc8.sln \
    QuantLibAddin_vc8.vcproj \
    README.txt \
    TODO.txt

.PHONY: docs docs-online docs-dist docs-clean
BASEPATH = @abs_top_srcdir@
docs:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-all
html-local:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-html
docs-online:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online
docs-dist:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-dist
docs-clean:
	$(MAKE) -C Docs docs-clean

