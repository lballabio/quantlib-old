# gensrc.py probably needs to be re-executed if any of these change

GENSRC_METADATA :=  metadata/Addins/doxygen.xml \
                    metadata/Addins/excel.xml \
                    metadata/Addins/loop.xml \
                    metadata/Addins/valueobjects.xml \
                    metadata/Functions/demo.xml \
                    metadata/Functions/garbagecollection.xml \
                    metadata/Functions/logging.xml \
                    metadata/Functions/ohutils.xml \
                    metadata/Functions/valueobjects.xml \
                    metadata/Rules/doxygen.xml \
                    metadata/Rules/excel.xml \
                    metadata/Rules/loop.xml \
                    metadata/Rules/valueobjects.xml \
                    metadata/Types/types.xml

GENSRC_STUBS :=     stubs/stub.copyright \
                    stubs/stub.excel.exports \
                    stubs/stub.excel.includes

GENSRC_CODE :=      code/codedict.py

GENSRC_CONF :=      config/config.xml

GENSRC_INPUTS = $(GENSRC_METADATA) $(GENSRC_STUBS) $(GENSRC_CODE) $(GENSRC_CONF)

# this should prevent running from a distributed tarball
EXTRA_DIST = .time-stamp \
             ohgensrc_vc8.vcproj \
             ohgensrc.vcproj \
             Makefile.vc \
             $(GENSRC_INPUTS)

all-local: .time-stamp

PYTHONPATH=./code
.time-stamp: $(GENSRC_INPUTS)
	python ../../gensrc/gensrc.py -xvld
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

