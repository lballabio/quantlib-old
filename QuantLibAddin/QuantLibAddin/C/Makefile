target = libQuantLibAddinC.a

objects  := $(patsubst %.cpp,%.o,$(wildcard *.cpp))
includes = -I../.. -I../../../ObjectHandler

all : $(target)

clean:
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ $(includes) -g -c -o $@ $<

$(target) : $(objects)
	ar rcs $@ $^
