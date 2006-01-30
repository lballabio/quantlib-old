BUILT_SOURCES = quantlib_wrap.cpp QuantLib_wrap.S

CLEANFILES = $(BUILT_SOURCES) .build-stamp

#if HAVE_R

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES) 
	R CMD SHLIB quantlib_wrap.cpp
	touch .build-stamp

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILD_SOURCES) quantlib_wrap.cxx

#endif

$(BUILT_SOURCES): ../SWIG/*.i
	$(SWIG) -r -c++ -Wall -v -I../SWIG quantlib.i
	mv quantlib_wrap.cxx quantlib_wrap.cpp

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt Makefile.PL $(BUILT_SOURCES)

