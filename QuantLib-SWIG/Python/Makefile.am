
CLEANFILES = QuantLib/quantlib_wrap.cpp QuantLib/QuantLib.py .build-stamp

BUILT_SOURCES = QuantLib/quantlib_wrap.cpp QuantLib/QuantLib.py

if HAVE_PYTHON

all-local: .build-stamp

.build-stamp: QuantLib/quantlib_wrap.cpp QuantLib/QuantLib.py
	$(PYTHON) setup.py build
	touch .build-stamp

check-local: .build-stamp
	$(PYTHON) setup.py test

install-exec-local: .build-stamp
	$(PYTHON) setup.py install

clean-local:
	rm -rf build

endif

QuantLib/quantlib_wrap.cpp QuantLib/QuantLib.py: ../SWIG/*.i
	$(SWIG) -python -c++ -modern -I../SWIG -outdir QuantLib \
            -o QuantLib/quantlib_wrap.cpp quantlib.i

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.py $(distdir)/examples
	mkdir -p $(distdir)/test
	cp ./test/*.py $(distdir)/test

EXTRA_DIST = README.txt setup.py QuantLib/__init__.py $(BUILT_SOURCES)

