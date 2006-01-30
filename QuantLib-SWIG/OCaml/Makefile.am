BUILT_SOURCES = quantlib_wrap.cpp QuantLib_wrap.S

CLEANFILES = $(BUILT_SOURCES) .build-stamp
OCAMLC = ocamlc -I /usr/lib/ocaml
#if HAVE_OCAML

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES) 
	$(OCAMLC) -c swig.mli
	$(OCAMLC) -c swig.ml
	$(OCAMLC) -c -ccopt QuantLib_wrap.cxx.c
	$(OCAMLC) -c QuantLib.mli
	$(OCAMLC) -c QuantLib.ml

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILD_SOURCES) *.ml *.mli *.cmli *.cmi *.cmo

#endif

$(BUILT_SOURCES): ../SWIG/*.i
	$(SWIG) -ocaml -co swig.mli
	$(SWIG) -ocaml -co swig.ml
	$(SWIG) -ocaml -c++ -Wall -v -I../SWIG quantlib.i
	mv quantlib_wrap.cxx quantlib_wrap.cxx.c

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt Makefile.PL $(BUILT_SOURCES)

