target = QuantLibAddin.a

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../..
libs :=	$(wildcard ../../ObjectClassLibrary/*.o)

all : $(target)

clean:
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<

$(target) : $(objects) $(libs)
	ar rcs $@ $^
