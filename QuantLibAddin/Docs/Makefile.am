
EXTRA_DIST = \
    docs-QuantLibAddin_vc8.vcproj \
    docs-QuantLibAddin_vc7.vcproj \
    Makefile.vc \
    qladdin.auto.doxy \
    qladdin.qlo.doxy \
    qladdin.root.doxy \
    qla_footer.html \
    qla_footer.subdir.html \
    qla_header.html \
    qla_headeronline.html \
    qla_header.subdir.html \
    qla_headeronline.subdir.html \
    style.css

DOXYGEN_CONFIG := qladdin.qlo.doxy
DOXYGEN_CUSTOM := style.css qla_footer.html qla_footer.subdir.html \
	qla_header.html qla_headeronline.html qla_header.subdir.html qla_headeronline.subdir.html
DOXYGEN_INPUT := $(shell find ${top_srcdir}/qlo -name *.hpp) \
             $(wildcard ../../ObjectHandler/Docs/pages/*.docs) \
             $(wildcard auto.pages/*.docs) \
             $(wildcard pages/*.docs)

%.gz: %
	rm -f $@
	gzip --best $<
%.tar: %
	tar chf $@ $<
	rm $<

.PHONY: docs-all docs-html docs-clean
.PHONY: docs-auto docs-qlo docs-root
.PHONY: docs-auto-online docs-qlo-online docs-root-online 
.PHONY: docs-dist docs-html-dist 

dist-hook:
	mkdir -p $(distdir)/pages $(distdir)/auto.pages $(distdir)/images
	cp -fp $(srcdir)/pages/*.docs $(distdir)/pages
	cp -fp $(srcdir)/auto.pages/*.docs $(distdir)/auto.pages
	cp -fp $(srcdir)/images/*.gif $(srcdir)/images/*.ico \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.png \
	      $(srcdir)/images/*.php \
	      $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: .time-stamp-auto .time-stamp-qlo .time-stamp-root
	mkdir -p html/images
	cp -f images/*.gif html/images
	cp -f images/*.ico html/images
	cp -f images/*.jpg html/images
	cp -f images/*.png html/images
	cp -f images/*.php html/images
	touch .time-stamp-html

docs-online: .time-stamp-html-online
.time-stamp-html-online: .time-stamp-auto-online .time-stamp-qlo-online .time-stamp-root-online
	mkdir -p html-online/images
	cp -f images/*.gif html-online/images
	cp -f images/*.ico html-online/images
	cp -f images/*.jpg html-online/images
	cp -f images/*.png html-online/images
	cp -f images/*.php html-online/images
	touch .time-stamp-html-online

docs-html-dist: docs-html
	ln -s html QuantLibAddin-docs-$(VERSION)-html
	tar czfvh QuantLibAddin-docs-$(VERSION)-html.tar.gz QuantLibAddin-docs-$(VERSION)-html
	rm QuantLibAddin-docs-$(VERSION)-html

docs-online-dist: docs-online
	ln -s html-online QuantLibAddin-docs-$(VERSION)-html
	tar czfvh QuantLibAddin-docs-online-$(VERSION)-html.tar.gz QuantLibAddin-docs-$(VERSION)-html
	rm QuantLibAddin-docs-$(VERSION)-html

docs-auto: .time-stamp-auto
.time-stamp-auto: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html
	$(SED) -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
               -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
               -e "s|qla_basepath|${BASEPATH}/|" \
       -e "s|qla_version|$(VERSION)|" \
       qladdin.auto.doxy > .qladdin.auto.doxy
	$(DOXYGEN) .qladdin.auto.doxy
	rm .qladdin.auto.doxy
	touch .time-stamp-auto

docs-qlo: .time-stamp-qlo
.time-stamp-qlo: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html
	$(SED) -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
               -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
               -e "s|qla_basepath|${BASEPATH}/|" \
       -e "s|qla_version|$(VERSION)|" \
       qladdin.qlo.doxy > .qladdin.qlo.doxy
	$(DOXYGEN) .qladdin.qlo.doxy
	rm .qladdin.qlo.doxy
	touch .time-stamp-qlo

docs-root: .time-stamp-root
.time-stamp-root: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html
	$(SED) -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
               -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
               -e "s|qla_basepath|${BASEPATH}/|" \
       -e "s|qla_version|$(VERSION)|" \
       qladdin.root.doxy > .qladdin.root.doxy
	$(DOXYGEN) .qladdin.root.doxy
	rm .qladdin.root.doxy
	touch .time-stamp-root

docs-auto-online: .time-stamp-auto-online
.time-stamp-auto-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html-online
	$(SED) -e "s/qla_header/qla_headeronline/" \
           -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
           qladdin.auto.doxy > .qladdin.auto.doxy
	$(DOXYGEN) .qladdin.auto.doxy
	rm .qladdin.auto.doxy
	touch .time-stamp-auto-online

docs-qlo-online: .time-stamp-qlo-online
.time-stamp-qlo-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html-online
	$(SED) -e "s/qla_header/qla_headeronline/" \
           -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
           qladdin.qlo.doxy > .qladdin.qlo.doxy
	$(DOXYGEN) .qladdin.qlo.doxy
	rm .qladdin.qlo.doxy
	touch .time-stamp-qlo-online

docs-root-online: .time-stamp-root-online
.time-stamp-root-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html-online
	$(SED) -e "s/qla_header/qla_headeronline/" \
           -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qla_basepath|${BASEPATH}/|" \
           -e "s|qla_version|$(VERSION)|" \
           qladdin.root.doxy > .qladdin.root.doxy
	$(DOXYGEN) .qladdin.root.doxy
	rm .qladdin.root.doxy
	touch .time-stamp-root-online

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

