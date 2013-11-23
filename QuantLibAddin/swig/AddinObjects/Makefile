
IDIR = ../../../../install/include

LIBHW = ../lib/libAddinObjectsHw.so
LIB = ../lib/libAddinObjects.so

all: $(LIBHW) $(LIB)

$(LIBHW): obj_hw.cpp obj_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIBHW) -I. -I.. -I$(IDIR) obj_hw.cpp

$(LIB): obj.cpp obj_hw.hpp
	g++ -shared -fPIC -g -Wall -o $(LIB) -I. -I.. -I$(IDIR) obj.cpp

clean:
	rm -f $(LIBHW) $(LIB)

