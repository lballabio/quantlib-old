
EXTRA_DIST = \
    qladdin.doxy \
    qla_footer.html \
    qla_footeronline.html \
    qla_header.html \
    QuantLibAddin-docs.hhp \
    quantlib.css

DOXYGEN_CONFIG := qladdin.doxy
DOXYGEN_CUSTOM := quantlib.css qla_header.html qla_footer.html
DOXYGEN_INPUT := $(shell find ${top_srcdir}/qla -name *.hpp) \
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
	cp -p $(srcdir)/images/*.bmp $(srcdir)/images/*.eps \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.png \
	      $(srcdir)/images/*.pdf $(srcdir)/images/*.ico $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: .time-stamp
	cp images/*.jpg html
	cp images/*.png html
	cp images/*.ico html
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html QuantLibAddin-docs-$(VERSION)-html
	tar chof - QuantLibAddin-docs-$(VERSION)-html \
	    | GZIP=--best gzip -c > QuantLibAddin-docs-$(VERSION)-html.tar.gz
	rm QuantLibAddin-docs-$(VERSION)-html

docs-online: .time-stamp-online
.time-stamp-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	$(SED) -e "s/qla_footer/qla_footeronline/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       qladdin.doxy > .qladdin.doxy
	$(DOXYGEN) .qladdin.doxy
	rm .qladdin.doxy
	cp images/*.jpg html-online
	cp images/*.png html-online
	cp images/*.ico html-online
	touch .time-stamp-online

docs-clean:
	rm -Rf html-online html man
	rm -f .time-stamp*

.time-stamp: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	$(SED) -e "s/GENERATE_MAN           = NO/GENERATE_MAN           = YES/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       qladdin.doxy > .qladdin.doxy
	$(DOXYGEN) .qladdin.doxy
	rm .qladdin.doxy
	touch .time-stamp

