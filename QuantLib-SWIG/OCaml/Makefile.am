
BUILT_SOURCES = swig.mli swig.ml QuantLib.mli QuantLib.ml quantlib_wrap.c

CLEANFILES = $(BUILT_SOURCES) .build-stamp
OCAMLCFLAGS = -I/usr/lib/ocaml

if HAVE_OCAML

%.cmi: %.mli
	$(OCAMLC) -c $<

.PRECIOUS: %.cmi

%.cmo: %.ml %.cmi
	$(OCAMLC) -c $*.ml

%.o: %.c
	$(OCAMLC) -c $<

all-local: .build-stamp

.build-stamp: QuantLib.cmo swig.cmo
	touch .build-stamp

QuantLib.cmo: swig.cmi

#check-local: .build-stamp

#install-exec-local: .build-stamp

clean-local:
	rm -rf *.cmi *.cmo *.o

endif

QuantLib.mli QuantLib.ml quantlib_wrap.c: ../SWIG/*.i
	$(SWIG) -ocaml -c++ -I../SWIG -o quantlib_wrap.c quantlib.i

swig.mli:
	$(SWIG) -ocaml -co swig.mli

swig.ml:
	$(SWIG) -ocaml -co swig.ml

#dist-hook:
#	mkdir -p $(distdir)/examples
#	cp ./examples/*.pl $(distdir)/examples

EXTRA_DIST = README.txt $(BUILT_SOURCES)

