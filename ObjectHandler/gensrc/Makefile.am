# gensrc.py probably needs to be re-executed if any of these change

GENSRC_INPUT := metadata/demo.xml \
                metadata/garbagecollection.xml \
                metadata/logging.xml \
                metadata/ohutils.xml \
                metadata/valueobjects.xml

GENSRC_STUBS := stubs/stub.copyright \
                stubs/stub.excel.exports \
                stubs/stub.excel.includes

# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             ohgensrc_vc8.vcproj \
             ohgensrc.vcproj \
             Makefile.vc

all-local: .time-stamp

.time-stamp: $(GENSRC_INPUT) $(GENSRC_STUBS)
	python ../../gensrc/gensrc.py -edvl
	touch .time-stamp

dist-hook:
	mkdir -p $(distdir)/config $(distdir)/metadata $(distdir)/scripts $(distdir)/stubs
	cp -p $(srcdir)/config/*.xml $(distdir)/config
	cp -p $(srcdir)/metadata/*.xml $(distdir)/metadata
	cp -p $(srcdir)/scripts/*.py $(distdir)/scripts
	cp -p $(srcdir)/stubs/stub.* $(distdir)/stubs
	touch .time-stamp

clean:
	rm -f .time-stamp

