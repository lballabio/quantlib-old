
SUBDIRS = \
    Docs

EXTRA_DIST = \
    Authors.txt \
    Contributors.txt \
    LICENSE.TXT \
    NEWS.txt \
    QuantLibXL-bin.nsi \
    QuantLibXL-src.nsi \
    QuantLibXL.sln \
    QuantLibXL_vc8.sln \
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

