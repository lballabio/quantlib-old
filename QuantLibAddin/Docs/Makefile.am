
EXTRA_DIST = \
    docs-QuantLibAddin_vc8.vcproj \
    docs-QuantLibAddin_vc7.vcproj \
    Makefile.vc \
    qladdin.root.doxy \
    qladdin.qlo.doxy \
    qladdin.auto.doxy \
    qla_footer.html \
    qla_footer.subdir.html \
    qla_header.html \
    qla_headeronline.html \
    qla_header.subdir.html \
    qla_headeronline.subdir.html \
    doxygen.css \
    tabs.css \
    ql.css

DOXYGEN_CONFIG := qladdin.qlo.doxy
DOXYGEN_CUSTOM := doxygen.css tabs.css ql.css qla_footer.html \
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
.PHONY: docs-root docs-qlo docs-auto
.PHONY: docs-root-online docs-qlo-online docs-auto-online
.PHONY: docs-dist docs-html-dist 

dist-hook:
	mkdir -p $(distdir)/pages $(distdir)/auto.pages $(distdir)/images
	cp -fp $(srcdir)/pages/*.docs $(distdir)/pages
	cp -fp $(srcdir)/auto.pages/*.docs $(distdir)/auto.pages
	cp -fp $(srcdir)/images/*.ico \
	      $(srcdir)/images/*.jpg $(srcdir)/images/*.png \
	      $(srcdir)/images/*.php \
	      $(distdir)/images

docs-all: docs-html
docs-dist: docs-html-dist

docs-html: .time-stamp-html
.time-stamp-html: .time-stamp-root .time-stamp-qlo .time-stamp-auto
	mkdir -p html/images
	cp tabs.css html
	cp ql.css html
	cp -f images/*.ico html/images
	cp -f images/*.jpg html/images
	cp -f images/*.png html/images
	cp -f images/*.php html/images
	mkdir -p html/qlo/images
	cp tabs.css html/qlo
	cp ql.css html/qlo
	cp -f images/*.ico html/qlo/images
	cp -f images/*.jpg html/qlo/images
	cp -f images/*.png html/qlo/images
	cp -f images/*.php html/qlo/images
	mkdir -p html/auto/images
	cp tabs.css html/auto
	cp ql.css html/auto
	cp -f images/*.ico html/auto/images
	cp -f images/*.jpg html/auto/images
	cp -f images/*.png html/auto/images
	cp -f images/*.php html/auto/images
	touch .time-stamp-html

docs-online: .time-stamp-html-online
.time-stamp-html-online: .time-stamp-root-online .time-stamp-qlo-online .time-stamp-auto-online
	cp tabs.css html-online
	cp ql.css html-online
	mkdir -p html-online/images
	cp -f images/*.ico html-online/images
	cp -f images/*.jpg html-online/images
	cp -f images/*.png html-online/images
	cp -f images/*.php html-online/images
	cp tabs.css html-online/qlo
	cp ql.css html-online/qlo
	mkdir -p html-online/qlo/images
	cp -f images/*.ico html-online/qlo/images
	cp -f images/*.jpg html-online/qlo/images
	cp -f images/*.png html-online/qlo/images
	cp -f images/*.php html-online/qlo/images
	cp tabs.css html-online/auto
	cp ql.css html-online/auto
	mkdir -p html-online/auto/images
	cp -f images/*.ico html-online/auto/images
	cp -f images/*.jpg html-online/auto/images
	cp -f images/*.png html-online/auto/images
	cp -f images/*.php html-online/auto/images
	touch .time-stamp-html-online

docs-html-dist: docs-html
	ln -s html QuantLibAddin-docs-$(VERSION)-html
	tar czfvh QuantLibAddin-docs-$(VERSION)-html.tar.gz QuantLibAddin-docs-$(VERSION)-html
	rm QuantLibAddin-docs-$(VERSION)-html

docs-online-dist: docs-online
	ln -s html-online quantlibaddin
	tar czfvh QuantLibAddin-docs-online-$(VERSION)-html.tar.gz quantlibaddin
	rm quantlibaddin

docs-root: .time-stamp-root
.time-stamp-root: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py qladdin.root.doxy .qladdin.root.doxy
	$(DOXYGEN) .qladdin.root.doxy
	rm .qladdin.root.doxy
	touch .time-stamp-root

docs-qlo: .time-stamp-qlo
.time-stamp-qlo: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py qladdin.qlo.doxy .qladdin.qlo.doxy
	$(DOXYGEN) .qladdin.qlo.doxy
	rm .qladdin.qlo.doxy
	touch .time-stamp-qlo

docs-auto: .time-stamp-auto
.time-stamp-auto: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py qladdin.auto.doxy .qladdin.auto.doxy
	$(DOXYGEN) .qladdin.auto.doxy
	rm .qladdin.auto.doxy
	touch .time-stamp-auto

docs-root-online: .time-stamp-root-online
.time-stamp-root-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py -o qladdin.root.doxy .qladdin.root.doxy
	$(DOXYGEN) .qladdin.root.doxy
	rm .qladdin.root.doxy
	touch .time-stamp-root-online

docs-qlo-online: .time-stamp-qlo-online
.time-stamp-qlo-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py -o qladdin.qlo.doxy .qladdin.qlo.doxy
	$(DOXYGEN) .qladdin.qlo.doxy
	rm .qladdin.qlo.doxy
	touch .time-stamp-qlo-online

docs-auto-online: .time-stamp-auto-online
.time-stamp-auto-online: $(DOXYGEN_CONFIG) $(DOXYGEN_CUSTOM) $(DOXYGEN_INPUT)
	python ../../ObjectHandler/dev_tools/preprocess_doxyfile.py -o qladdin.auto.doxy .qladdin.auto.doxy
	$(DOXYGEN) .qladdin.auto.doxy
	rm .qladdin.auto.doxy
	touch .time-stamp-auto-online

docs-clean:
	rm -Rf html html-online
	rm -f .time-stamp*

