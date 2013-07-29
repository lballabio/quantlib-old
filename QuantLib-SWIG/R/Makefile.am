QUANTLIB_R_ROOT = QuantLib

BUILT_SOURCES = src/$(QUANTLIB_R_ROOT).cpp R/$(QUANTLIB_R_ROOT).R

CLEANFILES = $(BUILT_SOURCES) .build-stamp

QUANTLIB_CONFIG=quantlib-config

if HAVE_R
if BUILD_R

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES)
	R CMD check .
	touch .build-stamp

check-local: .build-stamp

install-exec-local: .build-stamp
	R CMD INSTALL .

clean-local:
	rm -rf $(BUILT_SOURCES) ..Rcheck \
           src/symbols.rds src/QuantLib.o src/QuantLib.so

$(BUILT_SOURCES): ../SWIG/*.i
	$(SWIG) -r -c++ -o src/$(QUANTLIB_R_ROOT).cpp ../SWIG/quantlib.i
	mv src/$(QUANTLIB_R_ROOT).R R/

endif
endif

dist-hook:
	mkdir -p $(distdir)/demo
	cp ./demo/00Index $(distdir)/demo
	cp ./demo/*.R $(distdir)/demo

EXTRA_DIST = DESCRIPTION NAMESPACE README.txt \
             $(BUILT_SOURCES) \
             R/README src/Makevars

