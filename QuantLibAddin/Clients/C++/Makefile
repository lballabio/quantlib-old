target = QLADemo

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libdirs = 	-L../../QuantLibAddin
libs = -lQuantLib -lQuantLibAddin -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.cpp
	g++ -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<

$(target) : $(objects)
	g++ -g -O2 -o $@ $^ $(libdirs) $(libs)
