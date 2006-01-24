
EXTRA_DIST = \
    qladdin.doxy \
    qla_footer.html \
    qla_header.html \
    qla_headeronline.html \
    QuantLibAddin-docs.hhp \
    srcgen.doxy \
    srcgen_footer.html \
    srcgen_header.html \
    srcgen_headeronline.html \
    style.css

DOXYGEN_CONFIG := srcgen.doxy qladdin.doxy
DOXYGEN_CUSTOM := style.css \
    qla_footer.html qla_header.html qla_headeronline.html \
    srcgen_footer.html srcgen_header.html srcgen_headeronline.html
DOXYGEN_INPUT := $(shell find ${top_srcdir}/qla -name *.hpp) \
                 $(wildcard pages/*.docs) \
                 $(wildcard ../srcgen/*.py)

%.gz: %
	rm -f $@
	gzip --best $<
%.tar: %
	tar chf $@ $<
	rm $<

.PHONY: docs-all docs-html docs-online docs-clean
.PHONY: docs-qla docs-srcgen docs-online-qla docs-online-srcgen
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
	mkdir -p html/images
	cp images/*.gif html/images
	cp images/*.ico html/images
	cp images/*.jpg html/images
	cp images/*.png html/images
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html QuantLibAddin-docs-$(VERSION)-html
	tar chof - QuantLibAddin-docs-$(VERSION)-html \
	    | GZIP=--best gzip -c > QuantLibAddin-docs-$(VERSION)-html.tar.gz
	rm QuantLibAddin-docs-$(VERSION)-html

docs-online-qla:
	mkdir -p html-online
	$(SED) -e "s/qla_footer/qla_headeronline/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       qladdin.doxy > .qladdin.doxy
	$(DOXYGEN) .qladdin.doxy
	rm .qladdin.doxy

docs-online-srcgen:
	mkdir -p html-online/srcgen
	$(SED) -e "s/qla_footer/qla_headeronline/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       srcgen.doxy > .srcgen.doxy
	$(DOXYGEN) .srcgen.doxy
	rm .srcgen.doxy

docs-online: .time-stamp-online
.time-stamp-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT) docs-online-srcgen docs-online-qla
	mkdir -p html-online/images
	cp images/*.gif html-online/images
	cp images/*.ico html-online/images
	cp images/*.jpg html-online/images
	cp images/*.png html-online/images
	touch .time-stamp-online

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

docs-qla:
	mkdir -p html
	$(SED) -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       qladdin.doxy > .qladdin.doxy
	$(DOXYGEN) .qladdin.doxy
	rm .qladdin.doxy

docs-srcgen:
	mkdir -p html/srcgen
	$(SED) -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       srcgen.doxy > .srcgen.doxy
	$(DOXYGEN) .srcgen.doxy
	rm .srcgen.doxy

.time-stamp: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT) docs-srcgen docs-qla
	touch .time-stamp

