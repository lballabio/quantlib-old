
CLEANFILES = quantlib_wrap.cpp libQuantLibJNI.@JNILIB_EXTENSION@ QuantLib.jar

BUILT_SOURCES = quantlib_wrap.cpp

if HAVE_JAVA

all-local: libQuantLibJNI.@JNILIB_EXTENSION@ QuantLib.jar

quantlib_wrap.o: quantlib_wrap.cpp
	g++ -c quantlib_wrap.cpp -fno-strict-aliasing $(CXXFLAGS) @JDK_INCLUDE@ @JDK_SYSTEM_INCLUDE@ `quantlib-config --cflags` -o quantlib_wrap.o

libQuantLibJNI.@JNILIB_EXTENSION@: quantlib_wrap.o
	g++ @SHARED_LIB@ quantlib_wrap.o -Wl -o libQuantLibJNI.@JNILIB_EXTENSION@ `quantlib-config --libs`

QuantLib.jar: quantlib_wrap.cpp org/quantlib/*.java
	mkdir -p bin
	find org/quantlib -name '*.java' | xargs $(JAVAC) -d bin
	$(JAR) cf QuantLib.jar -C bin org

install-exec-local:
	cp -p libQuantLibJNI.@JNILIB_EXTENSION@ $(DESTDIR)/$(libdir)/libQuantLibJNI.@JNILIB_EXTENSION@
	cp -p QuantLib.jar $(DESTDIR)/$(libdir)/QuantLib.jar

check-local: examples/EquityOptions.class examples/DiscreteHedging.class
	LD_LIBRARY_PATH=. $(JAVA) -cp ".:QuantLib.jar" examples.DiscreteHedging
	LD_LIBRARY_PATH=. $(JAVA) -cp ".:QuantLib.jar" examples.EquityOptions

examples/DiscreteHedging.class: examples/DiscreteHedging.java QuantLib.jar
	$(JAVAC) -source 1.5 -cp QuantLib.jar examples/DiscreteHedging.java

examples/EquityOptions.class: examples/EquityOptions.java QuantLib.jar
	$(JAVAC) -source 1.5 -cp QuantLib.jar examples/EquityOptions.java

endif


quantlib_wrap.cpp: ../SWIG/*.i
	mkdir -p org/quantlib
	$(SWIG) -java -c++ -outdir org/quantlib \
            -package org.quantlib -o quantlib_wrap.cpp ../SWIG/quantlib.i

dist-hook:
	mkdir -p $(distdir)/org/quantlib
	cp ./org/quantlib/*.java $(distdir)/org/quantlib
	touch $(distdir)/quantlib_wrap.cpp
	mkdir -p $(distdir)/examples
	cp ./examples/*.java $(distdir)/examples

clean-local:
	rm -rf org 
	rm -f examples/*.class
	rm -f quantlib_wrap.o
	rm -rf bin

EXTRA_DIST = README.txt $(BUILT_SOURCES)

