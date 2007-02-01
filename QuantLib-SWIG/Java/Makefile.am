
CLEANFILES = quantlib_wrap.cpp libjquantlib.@LIB_EXTENSION@ QuantLib.jar

BUILT_SOURCES = quantlib_wrap.cpp

all: libjquantlib.@LIB_EXTENSION@ QuantLib.jar

quantlib_wrap.cpp: org/quantlib ../SWIG/*.i
	$(SWIG) -java -c++ -I../SWIG -outdir org/quantlib \
            -package org.quantlib -o quantlib_wrap.cpp quantlib.i

quantlib_wrap.o: quantlib_wrap.cpp
	g++ -c quantlib_wrap.cpp $(CXXFLAGS) @JDK_INCLUDE@ @JDK_SYSTEM_INCLUDE@ `quantlib-config --cflags` -o quantlib_wrap.o

libjquantlib.@LIB_EXTENSION@: quantlib_wrap.o
	g++ @SHARED_LIB@ quantlib_wrap.o -Wl -o libjquantlib.@LIB_EXTENSION@ `quantlib-config --libs`

QuantLib.jar: org/quantlib
	mkdir -p bin
	find org/quantlib -name '*.java' | xargs $(JAVAC) -d bin
	$(JAR) cf QuantLib.jar -C bin org

compile_test: QuantLib.jar
	$(JAVAC) -cp QuantLib.jar ./test/Hello.java

install-exec-local:
	cp libjquantlib.@LIB_EXTENSION@ $(DESTDIR)/$(libdir)/libjquantlib.@LIB_EXTENSION@
	cp QuantLib.jar $(DESTDIR)/$(libdir)/QuantLib.jar

org/quantlib:
	mkdir -p org/quantlib

dist-hook:
	mkdir -p $(distdir)/org/quantlib
	cp ./org/quantlib/*.java $(distdir)/org/quantlib
	touch $(distdir)/quantlib_wrap.cpp

clean-local:
	rm -rf org 
	rm -f test/Hello.class
	rm -f quantlib_wrap.o
	rm -rf bin

EXTRA_DIST = README.txt $(BUILT_SOURCES)

