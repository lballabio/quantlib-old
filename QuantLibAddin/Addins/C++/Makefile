target = libQuantLibAddin.a

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libs :=	$(wildcard ../../QuantLibAddin/*.o) \
		$(wildcard ../../../ObjectHandler/ObjectHandler/*.o)

all : $(target)

clean:
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<

$(target) : $(objects) $(libs)
	ar rcs $@ $^
