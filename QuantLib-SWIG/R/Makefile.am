BUILT_SOURCES = QuantLib.cpp QuantLib_wrap.S

CLEANFILES = $(BUILT_SOURCES) .build-stamp

PKG_LIBS="-L/usr/local/lib -lQuantLib"

#if HAVE_R

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES) 
	PKG_LIBS="-L/usr/local/lib -lQuantLib" R CMD SHLIB QuantLib.cpp
	touch .build-stamp

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILT_SOURCES) quantlib_wrap.cxx *.so *.o *.S

#endif

$(BUILT_SOURCES): ../SWIG/*.i
	$(SWIG) -r -c++ -Wall -v -I../SWIG quantlib.i
	mv quantlib_wrap.cxx QuantLib.cpp

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt Makefile.PL $(BUILT_SOURCES)

