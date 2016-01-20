
all: check-env libbtmalloc.so printTrace

check-env:
ifndef CXX
	$(error CXX is undefined)
endif
ifndef BOOST_INCLUDE
	$(error BOOST_INCLUDE is undefined)
endif

libbtmalloc.so: btmalloc.cpp
	$(CXX) --std=c++11 -g -shared -fPIC -o libbtmalloc.so btmalloc.cpp -ldl -lpthread

printTrace: printTrace.cpp
	$(CXX) --std=c++11 -o printTrace  -I$(BOOST_INCLUDE) printTrace.cpp

clean:
	rm printTrace *.o *.so
