sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I..

all : $(objects)

clean:
	rm -f $(objects)

$(objects) : %.o : %.cpp
	g++ -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<
