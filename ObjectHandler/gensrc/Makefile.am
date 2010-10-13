
GENSRC_DIR = @gensrc_dir@
OH_DIR = ..

# gensrc.py probably needs to be re-executed if any of these change

GENSRC_METADATA :=  metadata/addins/doxygen.xml \
                    metadata/addins/excel.xml \
                    metadata/addins/loop.xml \
                    metadata/addins/valueobjects.xml \
                    metadata/functions/enumerations.xml \
                    metadata/functions/garbagecollection.xml \
                    metadata/functions/logging.xml \
                    metadata/functions/ohutils.xml \
                    metadata/functions/valueobjects.xml \
                    metadata/rules/doxygen.xml \
                    metadata/rules/excel.xml \
                    metadata/rules/loop.xml \
                    metadata/rules/valueobjects.xml \
                    metadata/types/types.xml

GENSRC_STUBS :=     stubs/stub.copyright \
                    stubs/stub.excel.exports \
                    stubs/stub.excel.includes

GENSRC_CODE :=      code/codedict.py

GENSRC_CONF :=      config/config.xml

GENSRC_INPUTS = $(GENSRC_METADATA) $(GENSRC_STUBS) $(GENSRC_CODE) $(GENSRC_CONF)

# This should prevent running from a distributed tarball

EXTRA_DIST = .time-stamp \
             ohgensrc_vc8.vcproj \
             ohgensrc_vc7.vcproj \
             Makefile.vc \
             $(GENSRC_INPUTS)

all-local: .time-stamp

.time-stamp: $(GENSRC_INPUTS)
	python $(GENSRC_DIR)/gensrc.py -xdvl --oh_dir=$(OH_DIR)
	touch .time-stamp

dist-hook:
	mkdir -p $(distdir)/code
	mkdir -p $(distdir)/config
	mkdir -p $(distdir)/metadata
	mkdir -p $(distdir)/metadata/addins
	mkdir -p $(distdir)/metadata/functions
	mkdir -p $(distdir)/metadata/rules
	mkdir -p $(distdir)/metadata/types
	mkdir -p $(distdir)/stubs
	cp -pf $(srcdir)/config/*.xml $(distdir)/config
	cp -pf $(srcdir)/metadata/addins/*.xml $(distdir)/metadata/addins
	cp -pf $(srcdir)/metadata/functions/*.xml $(distdir)/metadata/functions
	cp -pf $(srcdir)/metadata/rules/*.xml $(distdir)/metadata/rules
	cp -pf $(srcdir)/metadata/types/*.xml $(distdir)/metadata/types
	cp -pf $(srcdir)/stubs/stub.* $(distdir)/stubs
	touch .time-stamp

clean:
	rm -f .time-stamp

DISTCLEANFILES = .time-stamp

