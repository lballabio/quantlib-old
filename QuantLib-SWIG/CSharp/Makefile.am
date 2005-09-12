
CLEANFILES = quantlib_wrap.cpp

BUILT_SOURCES = quantlib_wrap.cpp

clean-local:
	rm -rf csharp

quantlib_wrap.cpp: csharp ../SWIG/*.i
	$(SWIG) -csharp -c++ -I../SWIG -outdir csharp \
            -namespace QuantLib -o quantlib_wrap.cpp quantlib.i

csharp:
	mkdir -p csharp

dist-hook:
	mkdir -p $(distdir)/csharp
	cp ./csharp/*.cs $(distdir)/csharp
	mkdir -p $(distdir)/examples
	cp ./examples/*.cs $(distdir)/examples

EXTRA_DIST = README.txt $(BUILT_SOURCES)

