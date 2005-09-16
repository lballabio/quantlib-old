
CLEANFILES = quantlib_wrap.* libNQuantLibc.so QuantLib.dll

BUILT_SOURCES = quantlib_wrap.cpp

if HAVE_MCS

all-local: libNQuantLibc.so QuantLib.dll

libNQuantLibc.so: quantlib_wrap.o
	g++ -shared quantlib_wrap.o -o libNQuantLibc.so -lQuantLib

quantlib_wrap.o: quantlib_wrap.cpp
	g++ -c -fpic quantlib_wrap.cpp

QuantLib.dll: quantlib_wrap.cpp
	$(MCS) -nologo -target:library -out:QuantLib.dll csharp/*.cs

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

