target = QLADemo

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libdirs = 	-L../../QuantLibAddin
libs = -lQuantLibAddin -lQuantLib -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<

$(target) : $(objects)
	g++ -g -o $@ $^ $(libdirs) $(libs)
