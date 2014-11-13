
IDIR = ../../../../install/include

LIBHW = ../lib/libAddinObjectsHw.so
LIB = ../lib/libAddinObjects.so

all: $(LIBHW) $(LIB)

$(LIBHW): obj_hw.cpp obj_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIBHW) -I. -I.. -I$(IDIR) obj_hw.cpp

$(LIB): obj_quote.o obj_date.o
	g++ -shared -fPIC -g -Wall -o $(LIB) -I. -I.. -I$(IDIR) obj_quote.o obj_date.o

obj_quote.o: obj_quote.cpp obj_quote.hpp
	g++ -c -fPIC -g -Wall -o obj_quote.o -I. -I.. -I$(IDIR) obj_quote.cpp

obj_date.o: obj_date.cpp obj_date.hpp
	g++ -c -fPIC -g -Wall -o obj_date.o -I. -I.. -I$(IDIR) obj_date.cpp

clean:
	rm -f $(LIBHW) $(LIB) *.o

