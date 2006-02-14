
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

QLA_CONFIG := qladdin.doxy
QLA_CUSTOM := style.css \
    qla_footer.html qla_header.html qla_headeronline.html
QLA_INPUT := $(shell find ${top_srcdir}/qla -name *.hpp) \
                 $(wildcard pages/*.docs)

SRCGEN_CONFIG := srcgen.doxy
SRCGEN_CUSTOM := style.css \
    srcgen_footer.html srcgen_header.html srcgen_headeronline.html
SRCGEN_INPUT := $(wildcard ../srcgen/*.py)

%.gz: %
	rm -f $@
	gzip --best $<
%.tar: %
	tar chf $@ $<
	rm $<

.PHONY: docs-all docs-html docs-online docs-clean
.PHONY: docs-qla docs-srcgen docs-qla-online docs-srcgen-online
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
.time-stamp-html: .time-stamp-qla .time-stamp-srcgen
	mkdir -p html/images
	cp images/*.gif html/images
	cp images/*.ico html/images
	cp images/*.jpg html/images
	cp images/*.png html/images
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html QuantLibAddin-docs-$(VERSION)-html
	tar czfvh QuantLibAddin-docs-$(VERSION)-html.tar.gz QuantLibAddin-docs-$(VERSION)-html
	rm QuantLibAddin-docs-$(VERSION)-html

docs-qla-online: .time-stamp-qla-online
.time-stamp-qla-online: $(QLA_CONFIG) $(QLA_CUSTOM) $(QLA_INPUT)
	mkdir -p html-online
	$(SED) -e "s/qla_footer/qla_headeronline/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       qladdin.doxy > .qladdin.doxy
	$(DOXYGEN) .qladdin.doxy
	rm .qladdin.doxy
	touch .time-stamp-qla-online

docs-srcgen-online: .time-stamp-srcgen-online
.time-stamp-srcgen-online: $(SRCGEN_CONFIG) $(SRCGEN_CUSTOM) $(SRCGEN_INPUT)
	mkdir -p html-online/srcgen
	$(SED) -e "s/qla_footer/qla_headeronline/" \
           -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       srcgen.doxy > .srcgen.doxy
	$(DOXYGEN) .srcgen.doxy
	rm .srcgen.doxy
	touch .time-stamp-srcgen-online

docs-online: .time-stamp-online
.time-stamp-online: docs-qla-online docs-srcgen-online
	mkdir -p html-online/images
	cp images/*.gif html-online/images
	cp images/*.ico html-online/images
	cp images/*.jpg html-online/images
	cp images/*.png html-online/images
	touch .time-stamp-online

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

docs-qla: .time-stamp-qla
.time-stamp-qla: $(QLA_CONFIG) $(QLA_CUSTOM) $(QLA_INPUT)
	mkdir -p html
	$(SED) -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       qladdin.doxy > .qladdin.doxy
	$(DOXYGEN) .qladdin.doxy
	rm .qladdin.doxy
	touch .time-stamp-qla

docs-srcgen: .time-stamp-srcgen
.time-stamp-srcgen: $(SRCGEN_CONFIG) $(SRCGEN_CUSTOM) $(SRCGEN_INPUT)
	mkdir -p html/srcgen
	$(SED) -e "s/GENERATE_HTML          = NO/GENERATE_HTML          = YES/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
	       srcgen.doxy > .srcgen.doxy
	$(DOXYGEN) .srcgen.doxy
	rm .srcgen.doxy
	touch .time-stamp-srcgen

