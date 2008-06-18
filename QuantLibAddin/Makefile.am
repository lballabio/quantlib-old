
SUBDIRS = \
    gensrc \
    qlo/conversions \
    qlo/enumerations/constructors \
    qlo/enumerations/factories \
    qlo/enumerations/register \
    qlo/loop \
    qlo/serialization/create \
    qlo/serialization/register \
    qlo/serialization \
    qlo/valueobjects \
    qlo \
    Addins/Cpp \
    Addins/Calc \
    Clients/Cpp \
    Clients/CppInstrumentIn \
    Clients/CppSwapOut \
    Clients/Calc \
    Docs

EXTRA_DIST = \
    Announce.txt \
    Authors.txt \
    autogen.sh \
    configure.ac \
    Contributors.txt \
    LICENSE.TXT \
    NEWS.txt \
    QuantLibAddin.nsi \
    QuantLibAddin_vc7.sln \
    QuantLibAddin_vc8.sln \
    QuantLibObjects_vc8.vcproj \
    QuantLibObjects_vc7.vcproj \
    README.txt \
    todo.txt

.PHONY: docs docs-online docs-dist docs-clean
BASEPATH = @abs_top_srcdir@
docs:
	$(MAKE) -C gensrc
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-all
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

