
EXTRA_DIST = \
    docs-QuantLibXL_vc8.vcproj \
    docs-QuantLibXL.vcproj \
    Makefile.vc \
    qlxl_footer.html \
    qlxl_header.html \
    qlxl_header.subdir.html \
    quantlibxl.auto.doxy \
    quantlibxl.root.doxy \
    doxygen.css \
    tabs.css \
    ql.css

DOXYGEN_CONFIG := quantlibxl.auto.doxy
DOXYGEN_CUSTOM := doxygen.css tabs.css ql.css qlxl_footer.html \
                  qlxl_header.html qlxl_header.subdir.html \
                  qlxl_headeronline.html qlxl_headeronline.subdir.html
DOXYGEN_INPUT := $(wildcard pages/*.docs) \
                 $(wildcard ../../ObjectHandler/Docs/pages/*.docs) \
                 $(wildcard ../../QuantLibAddin/Docs/pages/*.docs) \
                 $(wildcard ../../QuantLibAddin/Docs/auto.pages/*.docs)

%.gz: %
	rm -f $@
	gzip --best $<
%.tar: %
	tar chf $@ $<
	rm $<

.PHONY: docs-all docs-html docs-clean
.PHONY: docs-auto docs-root
.PHONY: docs-auto-online docs-root-online 
.PHONY: docs-dist docs-html-dist 

dist-hook:
	mkdir -p $(distdir)/pages $(distdir)/images
	cp -fp $(srcdir)/pages/*.docs $(distdir)/pages
	cp -fp $(srcdir)/images/*.ico \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.png \
	      $(srcdir)/images/*.php \
	      $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: .time-stamp-auto .time-stamp-root
	mkdir -p html/images
	cp -f tabs.css html
	cp -f ql.css html
	cp -f images/*.ico html/images
	cp -f images/*.jpg html/images
	cp -f images/*.png html/images
	cp -f images/*.php html/images
	mkdir -p html/auto/images
	cp -f tabs.css html/auto
	cp -f ql.css html/auto
	cp -f images/*.ico html/auto/images
	cp -f images/*.jpg html/auto/images
	cp -f images/*.png html/auto/images
	cp -f images/*.php html/auto/images
	touch .time-stamp-html

docs-html-dist: docs-html
	ln -s html QuantLibXL-docs-$(VERSION)-html
	tar czfvh QuantLibXL-docs-$(VERSION)-html.tar.gz QuantLibXL-docs-$(VERSION)-html
	rm QuantLibXL-docs-$(VERSION)-html

docs-online: .time-stamp-html-online
.time-stamp-html-online: .time-stamp-auto-online .time-stamp-root-online
	mkdir -p html-online/images
	cp -f tabs.css html-online
	cp -f ql.css html-online
	cp -f images/*.ico html-online/images
	cp -f images/*.jpg html-online/images
	cp -f images/*.png html-online/images
	cp -f images/*.php html-online/images
	mkdir -p html-online/auto/images
	cp -f tabs.css html-online/auto
	cp -f ql.css html-online/auto
	cp -f images/*.ico html-online/auto/images
	cp -f images/*.jpg html-online/auto/images
	cp -f images/*.png html-online/auto/images
	cp -f images/*.php html-online/auto/images
	touch .time-stamp-html-online

docs-online-dist: docs-online
	ln -s html-online quantlibxl
	tar czfvh QuantLibXL-docs-online-$(VERSION)-html.tar.gz quantlibxl
	rm quantlibxl

docs-auto: .time-stamp-auto
.time-stamp-auto: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html
	$(SED) -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s|qlxl_basepath|${BASEPATH}/|" \
           -e "s|qlxl_version|$(VERSION)|" \
           quantlibxl.auto.doxy > .quantlibxl.auto.doxy
	$(DOXYGEN) .quantlibxl.auto.doxy
	rm .quantlibxl.auto.doxy
	touch .time-stamp-auto

docs-root: .time-stamp-root
.time-stamp-root: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html
	$(SED) -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s|qlxl_basepath|${BASEPATH}/|" \
           -e "s|qlxl_version|$(VERSION)|" \
           quantlibxl.root.doxy > .quantlibxl.root.doxy
	$(DOXYGEN) .quantlibxl.root.doxy
	rm .quantlibxl.root.doxy
	touch .time-stamp-root

docs-auto-online: .time-stamp-auto-online
.time-stamp-auto-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html-online
	$(SED) -e "s/qlxl_header/qlxl_headeronline/" \
           -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qlxl_basepath|${BASEPATH}/|" \
           -e "s|qlxl_version|$(VERSION)|" \
           quantlibxl.auto.doxy > .quantlibxl.auto.doxy
	$(DOXYGEN) .quantlibxl.auto.doxy
	rm .quantlibxl.auto.doxy
	touch .time-stamp-auto-online

docs-root-online: .time-stamp-root-online
.time-stamp-root-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	mkdir -p html-online
	$(SED) -e "s/qlxl_header/qlxl_headeronline/" \
           -e "s/HAVE_DOT               = NO/HAVE_DOT               = YES/" \
           -e "s/GENERATE_HTMLHELP      = YES/GENERATE_HTMLHELP      = NO/" \
           -e "s/HTML_OUTPUT            = html/HTML_OUTPUT            = html-online/" \
           -e "s|qlxl_basepath|${BASEPATH}/|" \
           -e "s|qlxl_version|$(VERSION)|" \
           quantlibxl.root.doxy > .quantlibxl.root.doxy
	$(DOXYGEN) .quantlibxl.root.doxy
	rm .quantlibxl.root.doxy
	touch .time-stamp-root-online

docs-clean:
	rm -Rf html-online html
	rm -f .time-stamp*

