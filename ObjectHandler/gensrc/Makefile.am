
GENSRC_DIR = @gensrc_dir@

# gensrc.py probably needs to be re-executed if any of these change

GENSRC_METADATA :=  metadata/Addins/doxygen.xml \
                    metadata/Addins/excel.xml \
                    metadata/Addins/loop.xml \
                    metadata/Addins/valueobjects.xml \
                    metadata/Functions/enumerations.xml \
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

# This should prevent running from a distributed tarball

EXTRA_DIST = .time-stamp \
             ohgensrc_vc8.vcproj \
             ohgensrc_vc7.vcproj \
             Makefile.vc \
             $(GENSRC_INPUTS)

all-local: .time-stamp

.time-stamp: $(GENSRC_INPUTS)
	python $(GENSRC_DIR)/gensrc.py -xd
	touch .time-stamp

dist-hook:
	mkdir -p $(distdir)/code
	mkdir -p $(distdir)/config
	mkdir -p $(distdir)/metadata
	mkdir -p $(distdir)/metadata/Addins
	mkdir -p $(distdir)/metadata/Functions
	mkdir -p $(distdir)/metadata/Rules
	mkdir -p $(distdir)/metadata/Types
	mkdir -p $(distdir)/stubs
	cp -pf $(srcdir)/config/*.xml $(distdir)/config
	cp -pf $(srcdir)/metadata/Addins/*.xml $(distdir)/metadata/Addins
	cp -pf $(srcdir)/metadata/Functions/*.xml $(distdir)/metadata/Functions
	cp -pf $(srcdir)/metadata/Rules/*.xml $(distdir)/metadata/Rules
	cp -pf $(srcdir)/metadata/Types/*.xml $(distdir)/metadata/Types
	cp -pf $(srcdir)/stubs/stub.* $(distdir)/stubs
	touch .time-stamp

clean:
	rm -f .time-stamp

DISTCLEANFILES = .time-stamp

