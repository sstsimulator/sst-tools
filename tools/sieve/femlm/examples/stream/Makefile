CXX=g++
APIDIR=../../api

mlmstream: mlmstream.o
	$(CXX) -O3 -o mlmstream mlmstream.o -L$(APIDIR) -Wl,-rpath=$(APIDIR) -lmlm

mlmstream.o: mlmstream.c
	$(CXX) -O3 -I$(APIDIR) -o mlmstream.o -c mlmstream.c

clean:
	mlmstream mlmstream.o

