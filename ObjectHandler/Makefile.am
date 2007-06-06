
SUBDIRS = \
    gensrc \
    oh/Conversions \
    oh/Enumerations \
    oh \
    ohxl \
    xlsdk \
    Examples/ExampleObjects/Enumerations \
    Examples/ExampleObjects/Library \
    Examples/ExampleObjects/Objects \
    Examples/ExampleObjects/ValueObjects \
    Examples/ExampleObjects \
    Examples/C++ \
    Examples/xl \
    Docs

EXTRA_DIST = \
    Announce.txt \
    Authors.txt \
    autogen.sh \
    Contributors.txt \
    LICENSE.TXT \
    NEWS.txt \
    ObjectHandler.nsi \
    ObjectHandler_vc7.sln \
    ObjectHandler_vc8.sln \
    ohlib_vc7.vcproj \
    ohlib_vc8.vcproj \
    README.txt \
    TODO.txt

.PHONY: docs docs-online docs-dist docs-clean
BASEPATH = @abs_top_srcdir@
docs:
	$(MAKE) -C gensrc
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-all
html-local:
	$(MAKE) -C gensrc
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-html
docs-dist:
	$(MAKE) -C gensrc
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-dist
docs-online:
	$(MAKE) -C gensrc
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online
docs-online-dist:
	$(MAKE) -C gensrc
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online-dist
docs-clean:
	$(MAKE) -C gensrc clean
	$(MAKE) -C Docs docs-clean

