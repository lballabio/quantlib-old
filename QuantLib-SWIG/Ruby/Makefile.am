
CLEANFILES = quantlib_wrap.* QuantLibc.so extension.mak .build-stamp

BUILT_SOURCES = quantlib_wrap.cpp

if HAVE_RUBY
if BUILD_RUBY

all-local: .build-stamp

.build-stamp: quantlib_wrap.cpp
	CXXFLAGS="$(CXXFLAGS) $(CXXWARNINGFLAGS)" $(RUBY) setup.rb build
	touch .build-stamp

check-local: .build-stamp
	$(RUBY) setup.rb test

install-exec-local: .build-stamp
	$(RUBY) setup.rb install

endif
endif

quantlib_wrap.cpp: ../SWIG/*.i
	$(SWIG) -ruby -c++ -o ./quantlib_wrap.cpp ../SWIG/quantlib.i

dist-hook:
	mkdir -p $(distdir)/examples
	cp ./examples/*.rb $(distdir)/examples
	mkdir -p $(distdir)/test
	cp ./test/*.rb $(distdir)/test

EXTRA_DIST = README.txt setup.rb QuantLib.rb $(BUILT_SOURCES)

