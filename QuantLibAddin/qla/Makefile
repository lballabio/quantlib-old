target = libQuantLibAddin.a

objects  := $(patsubst %.cpp,%.o,$(wildcard *.cpp))
objects2 := $(patsubst %.cpp,%.o,$(wildcard */*.cpp))
includes = -I.. -I../../ObjectHandler
libs :=	$(wildcard ../../ObjectHandler/ObjectHandler/*.o)

all : $(target)

$(objects2):
	cd objects && $(MAKE)
	cd functions && $(MAKE)

clean:
	cd objects && $(MAKE) clean
	cd functions && $(MAKE) clean
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<

$(target) : $(objects) $(objects2)
	ar rcs $@ $^ $(libs)
