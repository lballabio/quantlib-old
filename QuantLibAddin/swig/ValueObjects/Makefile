
IDIR = ../../../../install/include

LIBHW = ../lib/libValueObjectsHw.so
LIB = ../lib/libValueObjects.so

all: $(LIBHW) $(LIB)

$(LIBHW): vo_hw.cpp vo_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIBHW) -I$(IDIR) vo_hw.cpp

$(LIB): vo.cpp vo.hpp
	g++ -shared -fPIC -g -Wall -o $(LIB) -I$(IDIR) vo.cpp

clean:
	rm -f $(LIBHW) $(LIB)

