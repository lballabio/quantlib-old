
BUILT_SOURCES = swig.mli swig.ml QuantLib.mli QuantLib.ml quantlib_wrap.c

CLEANFILES = $(BUILT_SOURCES) .build-stamp
OCAMLCFLAGS = -I/usr/lib/ocaml

if HAVE_OCAML

all-local: .build-stamp

.build-stamp: $(BUILT_SOURCES)
	$(OCAMLC) -c swig.mli
	$(OCAMLC) -c swig.ml
	$(OCAMLC) -c -ccopt quantlib_wrap.c
	$(OCAMLC) -c QuantLib.mli
	$(OCAMLC) -c QuantLib.ml

check-local: .build-stamp

install-exec-local: .build-stamp

clean-local:
	rm -rf $(BUILD_SOURCES) *.ml *.mli *.cmli *.cmi *.cmo

endif

$(BUILT_SOURCES): ../SWIG/*.i
	$(SWIG) -ocaml -co swig.mli
	$(SWIG) -ocaml -co swig.ml
	$(SWIG) -ocaml -c++ -Wall -I../SWIG -o quantlib_wrap.c quantlib.i

#dist-hook:
#	mkdir -p $(distdir)/examples
#	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt $(BUILT_SOURCES)

