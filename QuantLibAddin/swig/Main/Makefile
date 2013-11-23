
IDIR = ../../../../install/include
LDIR = ../../../../install/lib

all: mainQuantLib mainQuantLibAddinHw mainQuantLibAddin

mainQuantLib: mainQuantLib.cpp
	g++ -g -Wall -o mainQuantLib mainQuantLib.cpp -I$(IDIR) -L$(LDIR) -lQuantLib

mainQuantLibAddinHw: mainQuantLibAddinHw.cpp
	g++ -fPIC mainQuantLibAddinHw.cpp -o mainQuantLibAddinHw -I.. -L../lib -lAddinCppHw

#	g++ -fPIC mainQuantLibAddinHw.cpp -o mainQuantLibAddinHw -I.. -L../lib -lAddinCppHw -L$(LDIR) -lObjectHandler -lboost_regex -lboost_serialization -lboost_system -lboost_filesystem

mainQuantLibAddin: mainQuantLibAddin.cpp ../lib/libAddinObjects.so
	g++ -fPIC mainQuantLibAddin.cpp -o mainQuantLibAddin -I.. -L../lib -lAddinCpp

clean:
	rm -f mainQuantLib mainQuantLibAddinHw mainQuantLibAddin

