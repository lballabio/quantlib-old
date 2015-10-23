ifndef F90C
F90C=gfortran
endif

ifndef CC
CC=gcc
endif

RTSUPP = w2f__types OAD_active OAD_cp OAD_tape OAD_rev

liblgmad.so: $(addsuffix .o, $(RTSUPP)) driver_lgm.o lgm.pre.xb.x2w.w2f.post.o
	${F90C} -shared -g -O3 -o $@ $^

lgm.pre.xb.x2w.w2f.post.f90 $(addsuffix .f90, $(RTSUPP)) iaddr.c : toolChain

toolChain : lgm.f90
	openad -c -m rj $<

plain: liblgm.so

liblgm.so: lgm.o
	${F90C} -shared -g -O3 -o liblgmad.so $^

%.o: %.f90
	${F90C} -g -O3 -o $@ -c $< -fpic

%.o: %.c
	${CC} -g -O3 -o $@ -c $< -fpic

clean:
	rm -f *.o *.so
	rm -f ad_template* OAD_* w2f__* iaddr*
	rm -f lgm.pre* *.B *.xaif *.o *.mod driver driverE *~

.PHONY: clean
