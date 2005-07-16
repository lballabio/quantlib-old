
SUBDIRS = oh Example Docs

EXTRA_DIST = \
    Announce.txt \
    Authors.txt \
    autogen.sh \
    Contributors.txt \
    LICENSE.TXT \
    makefile.mak \
    NEWS.txt \
    ObjectHandler.dev \
    ObjectHandler.dsp \
    ObjectHandler.dsw \
    ObjectHandler.nsi \
    ObjectHandler.sln \
    ObjectHandler_vc8.sln \
    ObjectHandler_vc8.vcproj \
    ObjectHandler.vcproj \
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

