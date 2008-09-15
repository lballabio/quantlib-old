
SUBDIRS = \
    Docs

EXTRA_DIST = \
    Authors.txt \
    Contributors.txt \
    LICENSE.TXT \
    NEWS.txt \
    QuantLibAllDynamic_vc7.sln \
    QuantLibAllDynamic_vc8.sln \
    QuantLibAllDynamic_vc9.sln \
    QuantLibXL_basic_vc7.sln \
    QuantLibXL_basic_vc8.sln \
    QuantLibXL_basic_vc9.sln \
    QuantLibXL-bin.nsi \
    QuantLibXL_full_vc7.sln \
    QuantLibXL_full_vc8.sln \
    QuantLibXL_full_vc9.sln \
    QuantLibXL.nsi \
    QuantLibXL-src.nsi \
    README.txt \
    todo.txt

.PHONY: docs docs-online docs-dist docs-clean
BASEPATH = @abs_top_srcdir@
docs:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-all
docs-dist:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-dist
docs-online:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online
docs-online-dist:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online-dist
docs-clean:
	$(MAKE) -C Docs docs-clean

