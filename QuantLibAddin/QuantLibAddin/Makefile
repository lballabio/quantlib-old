target = libQuantLibAddin.a

objects  := $(patsubst %.cpp,%.o,$(wildcard *.cpp))
objects2 := $(patsubst %.cpp,%.o,$(wildcard */*.cpp))
includes = -I.. -I../../ObjectHandler
libs :=	$(wildcard ../../ObjectHandler/ObjectHandler/*.o)

all : $(target)

$(objects2):
	cd objects && $(MAKE)
	cd C && $(MAKE)
	cd C++ && $(MAKE)

clean:
	cd objects && $(MAKE) clean
	cd C && $(MAKE) clean
	cd C++ && $(MAKE) clean
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<

$(target) : $(objects) $(objects2)
	ar rcs $@ $^ $(libs)
