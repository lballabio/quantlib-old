sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler

all : $(objects)

clean:
	rm -f $(objects)

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<
