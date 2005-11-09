
EXTRA_DIST = \
    ObjectHandler-docs.hhp \
    objecthandler.doxy \
    oh_footer.html \
    oh_header.html \
    oh_headeronline.html \
    style.css

DOXYGEN_CONFIG := objecthandler.doxy
DOXYGEN_CUSTOM := style.css oh_header.html oh_footer.html
DOXYGEN_INPUT := $(shell find ${top_srcdir} -name *.hpp) \
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
	cp -p $(srcdir)/images/*.gif $(srcdir)/images/*.ico \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.png \
	      $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: .time-stamp
	cp images/*.gif html
	cp images/*.ico html
	cp images/*.jpg html
	cp images/*.png html
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html ObjectHandler-docs-$(VERSION)-html
	tar chof - ObjectHandler-docs-$(VERSION)-html \
	    | GZIP=--best gzip -c > ObjectHandler-docs-$(VERSION)-html.tar.gz
	rm ObjectHandler-docs-$(VERSION)-html

docs-online: .time-stamp-online
.time-stamp-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	$(SED) -e "s/oh_header/oh_headeronline/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|oh_basepath|${BASEPATH}/|" \
           -e "s|oh_version|$(VERSION)|" \
	       objecthandler.doxy > .objecthandler.doxy
	$(DOXYGEN) .objecthandler.doxy
	rm .objecthandler.doxy
	cp images/*.gif html-online
	cp images/*.ico html-online
	cp images/*.jpg html-online
	cp images/*.png html-online
	touch .time-stamp-online

docs-clean:
	rm -Rf html-online html man
	rm -f .time-stamp*

.time-stamp: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	$(SED) -e "s/GENERATE_MAN           = NO/GENERATE_MAN           = YES/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s|oh_basepath|${BASEPATH}/|" \
           -e "s|oh_version|$(VERSION)|" \
	       objecthandler.doxy > .objecthandler.doxy
	$(DOXYGEN) .objecthandler.doxy
	rm .objecthandler.doxy
	touch .time-stamp

