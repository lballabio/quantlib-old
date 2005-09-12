
CLEANFILES = quantlib_wrap.cpp

BUILT_SOURCES = quantlib_wrap.cpp org/quantlib

clean-local:
	rm -rf org

quantlib_wrap.cpp: org/quantlib ../SWIG/*.i
	$(SWIG) -python -java -c++ -I../SWIG -outdir org/quantlib \
            -package org.quantlib -o quantlib_wrap.cpp quantlib.i

org/quantlib:
	mkdir -p org/quantlib

dist-hook:
	ln -f org $(distdir)/org

EXTRA_DIST = README.txt $(BUILT_SOURCES)

