target = QLADemo

sources := $(wildcard *.c)
objects := $(patsubst %.c,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libdirs =  -L../../QuantLibAddin -L../../Addins/C
libs = -lQuantLibAddinC -lQuantLibAddin -lQuantLib -lboost_regex

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.c
	gcc $(includes) -g -c -o $@ $<

$(target) : $(objects)
	gcc -g -o $@ $^ $(libdirs) $(libs)
