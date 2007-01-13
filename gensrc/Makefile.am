
SUBDIRS = \
    Docs

EXTRA_DIST = \
    Authors.txt \
    dependencies.vc \
    gensrc.nsi \
    gensrc.sln \
    gensrc.vcproj \
    gensrc_vc8.sln \
    gensrc_vc8.vcproj \
    LICENSE.TXT \
    NEWS.txt \
    README.txt \
    readme_vc.txt
 
dist-hook:
	mkdir -p $(distdir)/import $(distdir)/metadata $(distdir)/stubs
	cp -p $(srcdir)/import/*.py $(distdir)/import
	cp -p $(srcdir)/metadata/*.xml $(distdir)/metadata
	cp -p $(srcdir)/stubs/stub.* $(distdir)/stubs

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

