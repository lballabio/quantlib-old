target = QLADemo

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libdirs = 	-L../../qla
libs = -lQuantLibAddin -lQuantLib -lObjectHandler

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<

$(target) : $(objects)
	g++ -g -o $@ $^ $(libdirs) $(libs)
