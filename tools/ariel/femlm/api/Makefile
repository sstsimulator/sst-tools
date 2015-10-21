
CXX=g++

libmlm.so: mlm.o
	g++ -shared -O3 -g -fPIC -o libmlm.so mlm.o

mlm.o: mlm.c mlm.h
	g++ -O3 -c -o mlm.o -I./ -fPIC mlm.c

clean:
	rm libmlm.so mlm.o
