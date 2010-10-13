
EXTRA_DIST = \
    docs_vc8.vcproj \
    docs_vc7.vcproj \
    Makefile.vc \
    objecthandler.doxy \
    oh_footer.html \
    oh_header.html \
    oh_headeronline.html \
    doxygen.css \
    tabs.css \
    ql.css

FILENAME := ObjectHandler-docs-$(VERSION)-html
PROJECTNAME := objecthandler
PROJECTINITIALS := oh
DOXYGEN_INPUT := $(shell find ${top_srcdir} -name *.hpp) \
                 $(shell find ${top_srcdir}/Examples -name *.cpp) \
                 $(wildcard auto.pages/*.docs) \
                 $(wildcard pages/*.docs)
OH_DIR := ..

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

