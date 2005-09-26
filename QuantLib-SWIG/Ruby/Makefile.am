
CLEANFILES = quantlib_wrap.* QuantLibc.so extension.mak .build-stamp

BUILT_SOURCES = quantlib_wrap.cpp

if HAVE_RUBY

all-local: .build-stamp

.build-stamp: quantlib_wrap.cpp
	$(RUBY) setup.rb build
	touch .build-stamp

check-local: .build-stamp
	$(RUBY) setup.rb test

install-exec-local: .build-stamp
	$(RUBY) setup.rb install

endif

quantlib_wrap.cpp: ../SWIG/*.i
	$(SWIG) -ruby -c++ -I../SWIG -o ./quantlib_wrap.cpp quantlib.i

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.rb $(distdir)/examples
	mkdir -p $(distdir)/test
	cp ./test/*.rb $(distdir)/test

EXTRA_DIST = README.txt setup.rb QuantLib.rb $(BUILT_SOURCES)

