
SUBDIRS = \
    gensrc \
    oh/conversions \
    oh/enumerations \
    oh/loop \
    oh/valueobjects \
    oh \
    ohxl \
    xlsdk \
    xlsdk/x64 \
    Examples/ExampleObjects/Enumerations \
    Examples/ExampleObjects/Library \
    Examples/ExampleObjects/Objects \
    Examples/ExampleObjects/Serialization \
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
    ObjectHandler_vc8.sln \
    ObjectHandler_vc9.sln \
    ObjectHandler_vc10.sln \
    ObjectHandler_vc11.sln \
    ObjectHandler_vc12.sln \
    ohlib_vc8.vcproj \
    ohlib_vc9.vcproj \
    ohlib_vc10.vcxproj \
    ohlib_vc10.vcxproj.filters \
    ohlib_vc11.vcxproj \
    ohlib_vc11.vcxproj.filters \
    ohlib_vc12.vcxproj \
    ohlib_vc12.vcxproj.filters \
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

