
CLEANFILES = cpp/quantlib_wrap.* csharp/*.cs *.so *.dll \
             examples/*.so examples/*.dll examples/*.exe

BUILT_SOURCES = cpp/quantlib_wrap.cpp

if HAVE_MCS
if BUILD_MCS

all-local: libNQuantLibc.so NQuantLib.dll

libNQuantLibc.so: cpp/quantlib_wrap.o
	g++ -shared cpp/quantlib_wrap.o -o libNQuantLibc.so -lQuantLib

cpp/quantlib_wrap.o: $(BUILT_SOURCES)
	g++ -c -fpic $(CXXFLAGS) cpp/quantlib_wrap.cpp -o cpp/quantlib_wrap.o

NQuantLib.dll: $(BUILT_SOURCES)
	$(MCS) -nologo -target:library -out:NQuantLib.dll csharp/*.cs

check-local: examples/BermudanSwaption.exe \
             examples/EquityOption.exe \
             examples/libNQuantLibc.so examples/NQuantLib.dll
	./examples/BermudanSwaption.exe
	./examples/EquityOption.exe

examples/%.exe: examples/%.cs
	$(MCS) -nologo -target:exe -out:$@ -reference:NQuantLib.dll $<

examples/libNQuantLibc.so: libNQuantLibc.so
	cd examples && ln -sf ../libNQuantLibc.so

examples/NQuantLib.dll: NQuantLib.dll
	cd examples && ln -sf ../NQuantLib.dll

endif
endif

cpp/quantlib_wrap.cpp: ../SWIG/*.i
	$(SWIG) -csharp -c++ -outdir csharp \
            -namespace QuantLib -o cpp/quantlib_wrap.cpp ../SWIG/quantlib.i

dist-hook: cpp/quantlib_wrap.cpp
	mkdir -p $(distdir)/cpp
	cp ./cpp/*.vcproj $(distdir)/cpp
	cp ./cpp/*.vcxproj $(distdir)/cpp
	cp ./cpp/quantlib_wrap.cpp $(distdir)/cpp
	cp ./cpp/QuantlibWrapper.h ./cpp/QuantlibWrapper.cpp $(distdir)/cpp
	cp ./cpp/stdafx.h $(distdir)/cpp
	mkdir -p $(distdir)/csharp
	cp ./csharp/*.csproj $(distdir)/csharp
	cp ./csharp/*.cs $(distdir)/csharp
	mkdir -p $(distdir)/examples
	cp ./examples/*.csproj $(distdir)/examples
	cp ./examples/*.cs $(distdir)/examples

EXTRA_DIST = \
    QuantLib_vc8.sln \
    QuantLib_vc9.sln \
    QuantLib_vc10.sln \
    QuantLib_vc11.sln \
    QuantLib_vc12.sln \
    QuantLib.props \
    swig.cmd \
    README.txt

