target = QLDemo

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../..
libs = 	../../Addins/C++/QuantLibAddin.a \
		../../ObjectHandler/ObjectHandler.a
sharedlibs = -lQuantLib -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.cpp
	g++ -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<

$(target) : $(objects) $(libs)
	g++ -g -O2 -o $@ $^ $(sharedlibs)
