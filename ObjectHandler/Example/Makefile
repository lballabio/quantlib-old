target = Example

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I. -I..
libs = 	../ObjectHandler/ObjectHandler.a
sharedlibs = -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<

$(target) : $(objects) $(libs)
	g++ -g -O2 -o $@ $^ $(sharedlibs)
