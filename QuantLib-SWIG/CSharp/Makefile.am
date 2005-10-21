
CLEANFILES = cpp/quantlib_wrap.* csharp/*.cs libNQuantLibc.so NQuantLib.dll

BUILT_SOURCES = cpp/quantlib_wrap.cpp

if HAVE_MCS

all-local: libNQuantLibc.so NQuantLib.dll

libNQuantLibc.so: cpp/quantlib_wrap.o
	g++ -shared cpp/quantlib_wrap.o -o libNQuantLibc.so -lQuantLib

cpp/quantlib_wrap.o: $(BUILT_SOURCES)
	g++ -c -fpic cpp/quantlib_wrap.cpp -o cpp/quantlib_wrap.o

NQuantLib.dll: $(BUILT_SOURCES)
	$(MCS) -nologo -target:library -out:NQuantLib.dll csharp/*.cs

endif

cpp/quantlib_wrap.cpp: ../SWIG/*.i
	$(SWIG) -csharp -c++ -I../SWIG -outdir csharp \
            -namespace QuantLib -o cpp/quantlib_wrap.cpp quantlib.i

dist-hook: cpp/quantlib_wrap.cpp
	mkdir -p $(distdir)/cpp
	cp ./cpp/QuantlibWrapper.vcproj $(distdir)/cpp
	cp ./cpp/quantlib_wrap.cpp $(distdir)/cpp
	cp ./cpp/QuantlibWrapper.h ./cpp/QuantlibWrapper.cpp $(distdir)/cpp
	cp ./cpp/stdafx.h $(distdir)/cpp
	mkdir -p $(distdir)/csharp
	cp ./csharp/NQuantLib.csproj $(distdir)/csharp
	cp ./csharp/*.cs $(distdir)/csharp
	mkdir -p $(distdir)/examples
	cp ./examples/*.csproj $(distdir)/examples
	cp ./examples/*.cs $(distdir)/examples

EXTRA_DIST = QuantLib.sln swig.cmd README.txt

