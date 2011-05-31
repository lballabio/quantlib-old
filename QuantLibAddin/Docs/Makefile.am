
EXTRA_DIST = \
    docs-QuantLibAddin_vc7.vcproj \
    docs-QuantLibAddin_vc8.vcproj \
    docs-QuantLibAddin_vc9.vcproj \
    docs-QuantLibAddin_vc10.vcxproj \
    Makefile.vc \
    quantlibaddin.doxy \
    qla_footer.html \
    qla_header.html \
    qla_headeronline.html \
    doxygen.css \
    tabs.css \
    ql.css

FILENAME := QuantLibAddin-docs-$(VERSION)-html
PROJECTNAME := quantlibaddin
PROJECTINITIALS := qla
DOXYGEN_INPUT := $(shell find ${top_srcdir}/qlo -name *.hpp) \
             $(wildcard ../../ObjectHandler/Docs/pages/*.docs) \
             $(wildcard auto.pages/*.docs) \
             $(wildcard pages/*.docs)
OH_DIR := @oh_dir@

# From here on this file is identical for ObjectHandler, gensrc, QuantLibAddin, QuantLibXL

DOXYGEN_CONFIG := $(PROJECTNAME).doxy
DOXYGEN_CONFIG_TEMP := .$(DOXYGEN_CONFIG)
DOXYGEN_CUSTOM := doxygen.css tabs.css ql.css $(PROJECTINITIALS)_footer.html $(PROJECTINITIALS)_header.html $(PROJECTINITIALS)_headeronline.html 
SCRIPT := $(OH_DIR)/dev_tools/preprocess_doxyfile.py

%.gz: %
	rm -f $@
	gzip --best $<
%.tar: %
	tar chf $@ $<
	rm $<

.PHONY: docs-all docs-html docs-online docs-clean
.PHONY: docs-dist docs-html-dist 

dist-hook:
	mkdir -p $(distdir)/auto.pages $(distdir)/pages $(distdir)/images
	cp -p $(srcdir)/auto.pages/*.docs $(distdir)/auto.pages
	cp -p $(srcdir)/pages/*.docs $(distdir)/pages
	cp -p $(srcdir)/images/*.ico $(srcdir)/images/*.jpg $(srcdir)/images/*.png $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python $(SCRIPT) $(DOXYGEN_CONFIG) $(DOXYGEN_CONFIG_TEMP)
	$(DOXYGEN) $(DOXYGEN_CONFIG_TEMP)
	rm $(DOXYGEN_CONFIG_TEMP)
	cp tabs.css html
	cp ql.css html
	mkdir -p html/images
	cp -f images/*.ico images/*.jpg images/*.png html/images
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html $(FILENAME)
	tar czfvh $(FILENAME).tar.gz $(FILENAME)
	rm $(FILENAME)

docs-online: .time-stamp-online
.time-stamp-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python $(SCRIPT) -o $(DOXYGEN_CONFIG) $(DOXYGEN_CONFIG_TEMP)
	$(DOXYGEN) $(DOXYGEN_CONFIG_TEMP)
	rm $(DOXYGEN_CONFIG_TEMP)
	cp tabs.css html-online
	cp ql.css html-online
	mkdir -p html-online/images
	cp -f images/*.ico images/*.jpg images/*.png html-online/images
	touch .time-stamp-online

docs-online-dist: docs-online
	ln -s html-online $(PROJECTNAME)
	tar czfvh $(FILENAME)-online.tar.gz $(PROJECTNAME)
	rm $(PROJECTNAME)

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

