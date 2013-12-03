
IDIR = ../../../../install/include
LDIR = ../../../../install/lib

LIBHW = ../lib/libAddinCppHw.so
LIB = ../lib/libAddinCpp.so

all: $(LIBHW) $(LIB)

$(LIBHW): cpp_hw.cpp cpp_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIBHW) -I.. -I$(IDIR) cpp_hw.cpp -L../lib -lValueObjectsHw -lAddinObjectsHw -L$(LDIR) -lQuantLib -lObjectHandler -lboost_regex -lboost_serialization -lboost_system -lboost_filesystem

$(LIB): cpp_quote.cpp cpp_quote.hpp
	g++ -shared -fPIC -g -Wall -o $(LIB) -I.. -I$(IDIR) cpp_quote.cpp -L../lib -lValueObjects -lAddinObjects -L../lib -lValueObjectsHw -lAddinObjectsHw -L$(LDIR) -lQuantLib -lObjectHandler -lboost_regex -lboost_serialization -lboost_system -lboost_filesystem

clean:
	rm -f $(LIBHW) $(LIB)

