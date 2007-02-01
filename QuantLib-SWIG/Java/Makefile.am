
CLEANFILES = 

BUILT_SOURCES = quantlib_wrap.cpp libquantlib_javawrap compile_jar compile_test

quantlib_wrap.cpp: org/quantlib ../SWIG/*.i
	$(SWIG) -java -c++ -I../SWIG -outdir org/quantlib \
            -package org.quantlib -o quantlib_wrap.cpp quantlib.i

quantlib_wrap.o: quantlib_wrap.cpp
	g++ -c quantlib_wrap.cpp -I@JAVA_INCLUDE@ -I@JAVA_SYSTEM_INCLUDE@ -I/usr/include `quantlib-config --cflags` -o quantlib_wrap.o

libquantlib_javawrap: quantlib_wrap.o
	g++ @SHARED_LIB@ quantlib_wrap.o -Wl -o libquantlib_javawrap.@LIB_EXTENSION@ `quantlib-config --libs`

compile_jar: 
	mkdir -p bin
	find org/quantlib -name '*.java' | xargs @_JAVA_HOME_@/bin/javac -d bin
	@_JAVA_HOME_@/bin/jar cf quantlib-wrapper.jar -C bin org

compile_test: compile_jar
	@_JAVA_HOME_@/bin/javac -cp quantlib-wrapper.jar ./test/Hello.java

install-exec-local: libquantlib_javawrap.@LIB_EXTENSION@ compile_jar
	cp libquantlib_javawrap.@LIB_EXTENSION@ $(DESTDIR)/$(libdir)/libquantlib_javawrap.@LIB_EXTENSION@
	cp quantlib-wrapper.jar @_JAVA_HOME_@/lib/quantlib-wrapper.jar

org/quantlib:
	mkdir -p org/quantlib

dist-hook:
	mkdir -p $(distdir)/org/quantlib
	cp ./org/quantlib/*.java $(distdir)/org/quantlib
	touch $(distdir)/quantlib_wrap.cpp

clean:
	rm quantlib_wrap.cpp
	rm org/quantlib/* 
	rm quantlib-wrapper.jar
	rm ./test/Hello.class
	rm quantlib_wrap.o
	rm libquantlib_javawrap.@LIB_EXTENSION@ 
	rm bin/org/quantlib/* 

EXTRA_DIST = README.txt $(BUILT_SOURCES)

