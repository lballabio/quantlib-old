
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

DOXYGEN_CONFIG := objecthandler.doxy
DOXYGEN_CUSTOM := doxygen.css tabs.css ql.css oh_header.html oh_footer.html
DOXYGEN_INPUT := $(shell find ${top_srcdir} -name *.hpp) \
                 $(wildcard auto.pages/*.docs) \
                 $(wildcard pages/*.docs)

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
	cp -p $(srcdir)/images/*.ico \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.png \
	      $(srcdir)/images/*.php \
	      $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: .time-stamp
	cp tabs.css html
	cp ql.css html
	mkdir -p html/images
	cp -f images/*.ico html/images
	cp -f images/*.jpg html/images
	cp -f images/*.png html/images
	cp -f images/*.php html/images
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html ObjectHandler-docs-$(VERSION)-html
	tar czfvh ObjectHandler-docs-$(VERSION)-html.tar.gz ObjectHandler-docs-$(VERSION)-html
	rm ObjectHandler-docs-$(VERSION)-html

docs-online: .time-stamp-online
.time-stamp-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../dev_tools/preprocess_doxyfile.py -o objecthandler.doxy .objecthandler.doxy
	$(DOXYGEN) .objecthandler.doxy
	rm .objecthandler.doxy
	cp tabs.css html-online
	cp ql.css html-online
	mkdir -p html-online/images
	cp -f images/*.ico html-online/images
	cp -f images/*.jpg html-online/images
	cp -f images/*.png html-online/images
	cp -f images/*.php html-online/images
	touch .time-stamp-online

docs-online-dist: docs-online
	ln -s html-online objecthandler
	tar czfvh ObjectHandler-docs-online-$(VERSION)-html.tar.gz objecthandler
	rm objecthandler

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

.time-stamp: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../dev_tools/preprocess_doxyfile.py objecthandler.doxy .objecthandler.doxy
	$(DOXYGEN) .objecthandler.doxy
	rm .objecthandler.doxy
	touch .time-stamp

