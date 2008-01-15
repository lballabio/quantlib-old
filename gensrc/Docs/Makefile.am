
EXTRA_DIST = \
	docs_vc7.vcproj \
	docs_vc8.vcproj \
    gensrc.doxy \
    gs_footer.html \
    gs_header.html \
    gs_headeronline.html \
    Makefile.vc \
    doxygen.css \
    tabs.css \
    ql.css

DOXYGEN_CONFIG := gensrc.doxy
DOXYGEN_CUSTOM := doxygen.css tabs.css ql.css gs_header.html gs_headeronline.html gs_footer.html
DOXYGEN_INPUT := $(shell find ${top_srcdir} -name *.py) \
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
	mkdir -p $(distdir)/pages $(distdir)/images
	cp -p $(srcdir)/pages/*.docs $(distdir)/pages
	cp -p $(srcdir)/images/*.ico \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.php \
	      $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py gensrc.doxy .gensrc.doxy
	$(DOXYGEN) .gensrc.doxy
	rm .gensrc.doxy
	cp tabs.css html
	cp ql.css html
	mkdir -p html/images
	cp images/*.ico html/images
	cp images/*.jpg html/images
	cp images/*.php html/images
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html gensrc-docs-$(VERSION)-html
	tar czfvh gensrc-docs-$(VERSION)-html.tar.gz gensrc-docs-$(VERSION)-html
	rm gensrc-docs-$(VERSION)-html

docs-online: .time-stamp-online
.time-stamp-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py -o gensrc.doxy .gensrc.doxy
	$(DOXYGEN) .gensrc.doxy
	rm .gensrc.doxy
	cp tabs.css html-online
	cp ql.css html-online
	mkdir -p html-online/images
	cp images/*.ico html-online/images
	cp images/*.jpg html-online/images
	cp images/*.php html-online/images
	touch .time-stamp-online

docs-online-dist: docs-online
	ln -s html-online gensrc
	tar czfvh gensrc-docs-online-$(VERSION)-html.tar.gz gensrc
	rm gensrc

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

