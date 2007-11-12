
BUILT_SOURCES = QuantLib_wrap.cpp QuantLib.R

CLEANFILES = $(BUILT_SOURCES) .build-stamp

QUANTLIB_R_ROOT = QuantLib


#QUANTLIB_R_ROOT = QuantLib_wrap # for swig 1.3.31. or before

if HAVE_R

all-local: .build-stamp

.build-stamp: $(QUANTLIB_R_ROOT).RData QuantLib_wrap.so
	touch .build-stamp

$(QUANTLIB_R_ROOT).RData: $(QUANTLIB_R_ROOT).R
	R CMD BATCH makeRData.R

QuantLib_wrap.so: $(BUILT_SOURCES)
	MAKEFLAGS="CXXFLAGS=$(CXXFLAGS)" PKG_LIBS=`quantlib-config --libs` R CMD SHLIB QuantLib_wrap.cpp

debug-object: $(BUILT_SOURCES)
	g++ -I/usr/lib/R/include  -I/usr/local/include   -fPIC  -g -pipe -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -march=i586 -mtune=pentiumpro -fasynchronous-unwind-tables -c QuantLib_wrap.cpp -o QuantLib_wrap.o

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILT_SOURCES) QuantLib_wrap.cpp *.so *.o $(QUANTLIB_R_ROOT).RData *.Rout

$(BUILT_SOURCES): ../SWIG/*.i
	$(SWIG) -r -c++ -I../SWIG -o QuantLib_wrap.cpp quantlib.i

endif

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.R $(distdir)/examples

EXTRA_DIST = README.txt $(BUILT_SOURCES) makeRData.R

