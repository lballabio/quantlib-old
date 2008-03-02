
SUBDIRS = \
    Docs

EXTRA_DIST = \
    Authors.txt \
    Contributors.txt \
    dependencies.vc \
    gensrc.nsi \
    gensrc.py \
    gensrc_vc7.sln \
    gensrc_vc7.vcproj \
    gensrc_vc8.sln \
    gensrc_vc8.vcproj \
    LICENSE.TXT \
    NEWS.txt \
    README.txt \
    readme_vc.txt
 
dist-hook:
	mkdir -p $(distdir)/gensrc
	mkdir -p $(distdir)/gensrc/addins
	mkdir -p $(distdir)/gensrc/categories
	mkdir -p $(distdir)/gensrc/configuration
	mkdir -p $(distdir)/gensrc/enumerations
	mkdir -p $(distdir)/gensrc/exceptions
	mkdir -p $(distdir)/gensrc/functions
	mkdir -p $(distdir)/gensrc/parameters
	mkdir -p $(distdir)/gensrc/patterns
	mkdir -p $(distdir)/gensrc/rules
	mkdir -p $(distdir)/gensrc/serialization
	mkdir -p $(distdir)/gensrc/stubs
	mkdir -p $(distdir)/gensrc/types
	mkdir -p $(distdir)/gensrc/utilities
	cp -p $(srcdir)/gensrc/*.py $(distdir)/gensrc
	cp -p $(srcdir)/gensrc/addins/*.py $(distdir)/gensrc/addins
	cp -p $(srcdir)/gensrc/categories/*.py $(distdir)/gensrc/categories
	cp -p $(srcdir)/gensrc/configuration/*.py $(distdir)/gensrc/configuration
	cp -p $(srcdir)/gensrc/enumerations/*.py $(distdir)/gensrc/enumerations
	cp -p $(srcdir)/gensrc/exceptions/*.py $(distdir)/gensrc/exceptions
	cp -p $(srcdir)/gensrc/functions/*.py $(distdir)/gensrc/functions
	cp -p $(srcdir)/gensrc/parameters/*.py $(distdir)/gensrc/parameters
	cp -p $(srcdir)/gensrc/patterns/*.py $(distdir)/gensrc/patterns
	cp -p $(srcdir)/gensrc/rules/*.py $(distdir)/gensrc/rules
	cp -p $(srcdir)/gensrc/serialization/*.py $(distdir)/gensrc/serialization
	cp -p $(srcdir)/gensrc/stubs/stub.* $(distdir)/gensrc/stubs
	cp -p $(srcdir)/gensrc/types/*.py $(distdir)/gensrc/types
	cp -p $(srcdir)/gensrc/utilities/*.py $(distdir)/gensrc/utilities

.PHONY: docs docs-online docs-dist docs-clean
BASEPATH = @abs_top_srcdir@
docs:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-all
docs-dist:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-dist
docs-online:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online
docs-online-dist:
	BASEPATH=$(BASEPATH) $(MAKE) -C Docs docs-online-dist
docs-clean:
	$(MAKE) -C Docs docs-clean

