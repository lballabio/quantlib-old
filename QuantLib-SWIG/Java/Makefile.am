
CLEANFILES = quantlib_wrap.cpp

BUILT_SOURCES = quantlib_wrap.cpp

clean-local:
	rm -rf org

quantlib_wrap.cpp: org/quantlib ../SWIG/*.i
	$(SWIG) -java -c++ -I../SWIG -outdir org/quantlib \
            -package org.quantlib -o quantlib_wrap.cpp quantlib.i

org/quantlib:
	mkdir -p org/quantlib

dist-hook:
	mkdir -p $(distdir)/org/quantlib
	cp ./org/quantlib/*.java $(distdir)/org/quantlib

EXTRA_DIST = README.txt $(BUILT_SOURCES)

