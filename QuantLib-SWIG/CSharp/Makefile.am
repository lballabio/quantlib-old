
CLEANFILES = quantlib_wrap.* libQuantLibc.so QuantLib.Net.dll

BUILT_SOURCES = quantlib_wrap.cpp

if HAVE_MCS

all-local: libQuantLibc.so QuantLib.Net.dll

libQuantLibc.so: quantlib_wrap.o
	g++ -shared quantlib_wrap.o -o libQuantLibc.so -lQuantLib

quantlib_wrap.o: quantlib_wrap.cpp
	g++ -c -fpic quantlib_wrap.cpp

QuantLib.Net.dll: quantlib_wrap.cpp
	$(MCS) -nologo -target:library -out:QuantLib.Net.dll csharp/*.cs

endif

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

