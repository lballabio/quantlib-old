target = QLADemo

sources := $(wildcard *.c)
objects := $(patsubst %.c,%.o,$(sources))
includes = -I../.. -I../../../ObjectHandler
libdirs =  -L../../qla -L../../Addins/C
libs = -lQuantLibAddinC -lQuantLibAddin -lQuantLib -lObjectHandler

all : $(target)

clean:
	rm -f $(target) $(objects) *.log

$(objects) : %.o : %.c
	gcc $(includes) -g -c -o $@ $<

$(target) : $(objects)
	gcc -g -o $@ $^ $(libdirs) $(libs)
