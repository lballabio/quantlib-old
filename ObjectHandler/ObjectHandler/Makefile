target = ObjectHandler.a

sources := $(wildcard *.cpp)
objects := $(patsubst %.cpp,%.o,$(sources))

all : $(target)

clean:
	rm -f $(target) $(objects)

$(objects) : %.o : %.cpp
	g++ -I.. -DHAVE_CONFIG_H -g -c -o $@ $<

$(target) : $(objects)
	ar rcs $@ $^
