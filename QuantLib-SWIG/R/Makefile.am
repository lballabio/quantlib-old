if HAVE_RSWIG
BUILT_SOURCES = QuantLib.cpp QuantLib_wrap.R
else
BUILT_SOURCES =
endif

CLEANFILES = $(BUILT_SOURCES) .build-stamp

if HAVE_R

all-local: .build-stamp

.build-stamp: QuantLib.so
	touch .build-stamp

QuantLib.so: $(BUILT_SOURCES)
	PKG_LIBS="-L/usr/local/lib -lQuantLib" R CMD SHLIB QuantLib.cpp

debug-object: $(BUILT_SOURCES)
	g++ -I/usr/lib/R/include  -I/usr/local/include   -fPIC  -g -pipe -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -march=i586 -mtune=pentiumpro -fasynchronous-unwind-tables -c QuantLib.cpp -o QuantLib.o

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILT_SOURCES) QuantLib.cpp *.so *.o QuantLib.RData *.Rout

endif

if HAVE_RSWIG
$(BUILT_SOURCES): ../SWIG/*.i
	$(RSWIG) -r -c++ -I../SWIG -o QuantLib.cpp quantlib.i

compiled-wrapper: QuantLib.RData

QuantLib.RData: QuantLib.so QuantLib_wrap.R
	R CMD BATCH makeRData.R
endif

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.R $(distdir)/examples

EXTRA_DIST = README.txt $(BUILT_SOURCES)

