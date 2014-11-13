
IDIR = ../../../../install/include
SYS_LIBS = -L../../../../install/lib -lQuantLib -lObjectHandler -lboost_regex -lboost_serialization -lboost_system -lboost_filesystem

LIBHW = ../lib/libAddinCppHw.so
LIB = ../lib/libAddinCpp.so

all: $(LIBHW) $(LIB)

$(LIBHW): cpp_hw.cpp cpp_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIBHW) -I.. -I$(IDIR) cpp_hw.cpp -L../lib -lValueObjectsHw -lAddinObjectsHw $(SYS_LIBS)

$(LIB): cpp_quote.o cpp_date.o
	g++ -shared -fPIC -g -Wall -o $(LIB) -I.. -I$(IDIR) cpp_quote.o cpp_date.o -L../lib -lValueObjects -lAddinObjects -L../lib -lValueObjectsHw -lAddinObjectsHw $(SYS_LIBS)

cpp_quote.o: cpp_quote.cpp cpp_quote.hpp
	g++ -c -fPIC -g -Wall -o cpp_quote.o -I.. -I$(IDIR) cpp_quote.cpp

cpp_date.o: cpp_date.cpp cpp_date.hpp
	g++ -c -fPIC -g -Wall -o cpp_date.o -I.. -I$(IDIR) cpp_date.cpp

clean:
	rm -f $(LIBHW) $(LIB) *.o

