QUANTLIB_R_ROOT = QuantLib

#QUANTLIB_R_ROOT = QuantLib_wrap # for swig 1.3.31. or before

BUILT_SOURCES = src/$(QUANTLIB_R_ROOT).cpp R/$(QUANTLIB_R_ROOT).R

CLEANFILES = $(BUILT_SOURCES) .build-stamp

QUANTLIB_CONFIG=quantlib-config

if HAVE_R
if BUILD_R

all-local: .build-stamp

#.build-stamp: $(QUANTLIB_R_ROOT).RData $(QUANTLIB_R_ROOT).so
.build-stamp: src/$(QUANTLIB_R_ROOT).cpp
	touch .build-stamp

#$(QUANTLIB_R_ROOT).RData: R/$(QUANTLIB_R_ROOT).R
#	R CMD BATCH makeRData.R

#$(QUANTLIB_R_ROOT).so: $(BUILT_SOURCES)
#	MAKEFLAGS="CPPFLAGS="`$(QUANTLIB_CONFIG) --cflags` PKG_LIBS=`$(QUANTLIB_CONFIG) --libs` R CMD SHLIB $(QUANTLIB_R_ROOT).cpp

debug-object: $(BUILT_SOURCES)
	g++ -I/usr/lib/R/include  -I/usr/local/include   -fPIC  -g -pipe -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -march=i586 -mtune=pentiumpro -fasynchronous-unwind-tables -c $(QUANTLIB_R_ROOT).cpp -o $(QUANTLIB_R_ROOT).o

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILT_SOURCES) $(QUANTLIB_R_ROOT).cpp *.so *.o $(QUANTLIB_R_ROOT).RData *.Rout

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
             $(BUILT_SOURCES) makeRData.R \
             R/README src/Makevars

