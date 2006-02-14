
if HAVE_RSWIG
BUILT_SOURCES = QuantLib.cpp QuantLib_wrap.S
else
BUILT_SOURCES =
endif

CLEANFILES = $(BUILT_SOURCES) .build-stamp

if HAVE_R

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES)
	PKG_LIBS="-L/usr/local/lib -lQuantLib" R CMD SHLIB QuantLib.cpp
	touch .build-stamp

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILT_SOURCES) quantlib_wrap.cxx *.so *.o *.S

endif

if HAVE_RSWIG
$(BUILT_SOURCES): ../SWIG/*.i
	$(RSWIG) -r -c++ -Wall -v -I../SWIG -o QuantLib.cpp quantlib.i
endif

#dist-hook:
#	mkdir -p $(distdir)/examples
#	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt $(BUILT_SOURCES)

