
IDIR = ../../../../install/include

LIBHW = ../lib/libValueObjectsHw.so
LIB = ../lib/libValueObjects.so

all: $(LIBHW) $(LIB)

$(LIBHW): vo_hw.cpp vo_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIBHW) -I$(IDIR) vo_hw.cpp

$(LIB): vo_quote.o vo_date.o
	g++ -shared -fPIC -g -Wall -o $(LIB) -I$(IDIR) vo_quote.o vo_date.o

vo_quote.o: vo_quote.cpp vo_quote.hpp
	g++ -c -fPIC -g -Wall -o vo_quote.o -I$(IDIR) vo_quote.cpp

vo_date.o: vo_date.cpp vo_date.hpp
	g++ -c -fPIC -g -Wall -o vo_date.o -I$(IDIR) vo_date.cpp

clean:
	rm -f $(LIBHW) $(LIB) *.o

