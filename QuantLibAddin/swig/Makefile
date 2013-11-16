
IDIR = ../../../install
SWIG = ../../../install/bin/swig

all: mainQuantLib mainQuantLibAddinTest mainQuantLibAddin

mainQuantLib: mainQuantLib.cpp
	g++ -g -Wall -o mainQuantLib mainQuantLib.cpp -I$(IDIR)/include -L$(IDIR)/lib -lQuantLib

libQuantLibAddinTest.so: quantLibAddinTest.cpp quantLibAddinTest.hpp
	g++ -shared -fPIC -g -Wall -o libQuantLibAddinTest.so quantLibAddinTest.cpp -I$(IDIR)/include -L$(IDIR)/lib -lQuantLib -lObjectHandler

mainQuantLibAddinTest: mainQuantLibAddinTest.cpp libQuantLibAddinTest.so
	g++ -fPIC mainQuantLibAddinTest.cpp -o mainQuantLibAddinTest -L. -lQuantLibAddinTest

QuantLibAddin.cpp: quantlib.i ../../../swig
	$(SWIG) -prefix ql -debug-top 4 -c++ -objecthandler quantlib.i

libQuantLibAddin.so: QuantLibAddin.cpp QuantLibAddin.hpp
	g++ -shared -fPIC -g -Wall -o libQuantLibAddin.so QuantLibAddin.cpp -L. -lQuantLib -I$(IDIR)/include -L$(IDIR)/lib -lObjectHandler

mainQuantLibAddin: mainQuantLibAddin.cpp libQuantLibAddin.so
	g++ -fPIC mainQuantLibAddin.cpp -o mainQuantLibAddin -L. -lQuantLibAddin

clean:
	rm -f *.so mainQuantLib mainQuantLibAddinTest mainQuantLibAddin QuantLibAddin.?pp

