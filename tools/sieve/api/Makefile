
libarielapi.so: libarielapi.o
	gcc -shared -o libarielapi.so libarielapi.o

libarielapi.o: libarielapi.c
	gcc -o libarielapi.o -fPIC -I./ -c libarielapi.c

clean:
	rm *.o *.so
