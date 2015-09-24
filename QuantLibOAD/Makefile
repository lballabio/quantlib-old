ifndef F90C
F90C=gfortran
endif

RTSUPP = w2f__types OAD_active

libsimplelibad.so: $(addsuffix .o, $(RTSUPP)) driver_simplelib.o simplelib.pre.xb.x2w.w2f.post.o
	${F90C} -shared -g -O3 -o $@ $^

simplelib.pre.xb.x2w.w2f.post.f90 $(addsuffix .f90, $(RTSUPP)) : toolChain

toolChain : simplelib.f90
	openad -c -m f $<

plain: libsimplelib.so

libsimplelib.so: simplelib.o
	${F90C} -shared -o $@ $^

%.o: %.f90
	${F90C} -g -O3 -o $@ -c $< -fpic

clean:
	rm -f *.o *.so
	rm -f ad_template* OAD_* w2f__* iaddr*
	rm -f simplelib.pre* *.B *.xaif *.o *.mod driver driverE *~

.PHONY: clean
