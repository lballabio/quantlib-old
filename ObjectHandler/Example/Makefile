target = Example

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I. -I..
libdirs = -L../ObjectHandler
libs = -lObjectHandler -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<

$(target) : $(objects) $(objects2)
	g++ -g -O2 -o $@ $^ $(libdirs) $(libs)
