target = QLADemo

sources := $(wildcard *.c)
objects := $(patsubst %.c,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libdirs = 	-L../../QuantLibAddin
libs = -lQuantLib -lQuantLibAddin -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.c
	gcc -DHAVE_CONFIG_H $(includes) -g -c -o $@ $<

$(target) : $(objects)
	gcc -g -O2 -o $@ $^ $(libdirs) $(libs)
