libarielfort.so: libarielfort.o
	gfortran -shared -o libarielfort.so libarielfort.o

libarielfort.o: arielfort.f90
	gfortran -o libarielfort.o -fPIC -I./ -c arielfort.f90

clean:
	rm *.o *.so *.mod
