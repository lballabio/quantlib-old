
IDIR = ../../../../install/include
LDIR = ../../../../install/lib

all: mainQuantLib mainQuantLibAddinHw mainQuantLibAddin

mainQuantLib: mainQuantLib.cpp
	g++ -g -Wall -o mainQuantLib mainQuantLib.cpp -I$(IDIR) -L$(LDIR) -lQuantLib

mainQuantLibAddinHw: mainQuantLibAddinHw.cpp
	g++ -fPIC mainQuantLibAddinHw.cpp -o mainQuantLibAddinHw -I.. -L../lib -lAddinCppHw

mainQuantLibAddin: mainQuantLibAddin.cpp ../lib/libAddinObjects.so
	g++ -fPIC mainQuantLibAddin.cpp -o mainQuantLibAddin -I.. -L../lib -lAddinCpp

clean:
	rm -f mainQuantLib mainQuantLibAddinHw mainQuantLibAddin

