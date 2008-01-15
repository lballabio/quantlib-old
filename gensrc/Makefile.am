
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
	mkdir -p $(distdir)/gensrc/Addins
	mkdir -p $(distdir)/gensrc/Categories
	mkdir -p $(distdir)/gensrc/Configuration
	mkdir -p $(distdir)/gensrc/Enumerations
	mkdir -p $(distdir)/gensrc/Exceptions
	mkdir -p $(distdir)/gensrc/Functions
	mkdir -p $(distdir)/gensrc/Parameters
	mkdir -p $(distdir)/gensrc/Patterns
	mkdir -p $(distdir)/gensrc/Rules
	mkdir -p $(distdir)/gensrc/Serialization
	mkdir -p $(distdir)/gensrc/stubs
	mkdir -p $(distdir)/gensrc/Types
	mkdir -p $(distdir)/gensrc/Utilities
	cp -p $(srcdir)/gensrc/*.py $(distdir)/gensrc
	cp -p $(srcdir)/gensrc/Addins/*.py $(distdir)/gensrc/Addins
	cp -p $(srcdir)/gensrc/Categories/*.py $(distdir)/gensrc/Categories
	cp -p $(srcdir)/gensrc/Configuration/*.py $(distdir)/gensrc/Configuration
	cp -p $(srcdir)/gensrc/Enumerations/*.py $(distdir)/gensrc/Enumerations
	cp -p $(srcdir)/gensrc/Exceptions/*.py $(distdir)/gensrc/Exceptions
	cp -p $(srcdir)/gensrc/Functions/*.py $(distdir)/gensrc/Functions
	cp -p $(srcdir)/gensrc/Parameters/*.py $(distdir)/gensrc/Parameters
	cp -p $(srcdir)/gensrc/Patterns/*.py $(distdir)/gensrc/Patterns
	cp -p $(srcdir)/gensrc/Rules/*.py $(distdir)/gensrc/Rules
	cp -p $(srcdir)/gensrc/Serialization/*.py $(distdir)/gensrc/Serialization
	cp -p $(srcdir)/gensrc/stubs/stub.* $(distdir)/gensrc/stubs
	cp -p $(srcdir)/gensrc/Types/*.py $(distdir)/gensrc/Types
	cp -p $(srcdir)/gensrc/Utilities/*.py $(distdir)/gensrc/Utilities

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

