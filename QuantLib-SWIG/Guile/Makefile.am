
CLEANFILES = quantlib_wrap.* QuantLibc.so QuantLib.scm test.* .build-stamp

BUILT_SOURCES = quantlib_wrap.cpp QuantLib.scm

if HAVE_GUILE

all-local: .build-stamp

.build-stamp: quantlib_wrap.cpp QuantLib.scm
	CXXFLAGS="$(CXXFLAGS)" $(GUILE) -s setup.scm build
	touch .build-stamp

check-local: .build-stamp
	$(GUILE) -s setup.scm test

install-exec-local: .build-stamp
	$(GUILE) -s setup.scm install

endif

quantlib_wrap.cpp QuantLib.scm: ../SWIG/*.i
	$(SWIG) -guile -c++ -Linkage passive -scmstub -gh -I../SWIG \
            -o quantlib_wrap.cpp quantlib.i

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.scm $(distdir)/examples
	mkdir -p $(distdir)/test
	cp ./test/*.scm $(distdir)/test

EXTRA_DIST = README.txt setup.scm $(BUILT_SOURCES)

