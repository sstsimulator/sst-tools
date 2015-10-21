#!/bin/bash

OS=$(shell uname -s)

SST_VERSION=${shell sst --version | sed -e 's/^[^(]*(\([^,]*\),.*/\1/g' }
SST_PKGCONFIG="SST-${SST_VERSION}"

ifeq ("${INTEL_PIN_DIRECTORY}", "")
PINDIR=${HOME}/pin/2.13-gcc/
else
PINDIR=${INTEL_PIN_DIRECTORY}
endif


CXX=g++
LD=g++
CC=gcc
CPPFLAGS=-DBIGARRAY_MULTIPLIER=1 -DUSING_XED -DTARGET_IA32E -DHOST_IA32E -fPIC
CPPFLAGS+= \
	-I $(PINDIR)/source/include/pin \
	-I $(PINDIR)/source/include/pin/gen \
	-I $(PINDIR)/extras/xed2-intel64/include \
	-I $(PINDIR)/extras/components/include 

LDFLAGS=-shared 
LDLIBS= \
		-L$(PINDIR)/extras/xed2-intel64/lib \
		-L$(PINDIR)/intel64/lib \
		-L$(PINDIR)/intel64/lib-ext


ifeq ("${OS}", "Darwin")
CPPFLAGS+=-DTARGET_MAC
LDFLAGS+= -Wl,-exported_symbols_list \
		  -Wl,${PINDIR}/source/include/pin/pintool.exp \
		  -stdlib=libstdc++
LDLIBS+=-lpin -lxed -lpindwarf -ldl
else
CPPFLAGS+=-DTARGET_LINUX
LDFLAGS+= -Wl,--hash-style=sysv -Wl,-Bsymbolic \
		  -Wl,--version-script=$(PINDIR)/source/include/pin/pintool.ver
LDLIBS+=-lpin -lxed -ldwarf -lelf -ldl -lrt
endif

CPPFLAGS += $(shell pkg-config --cflags ${SST_PKGCONFIG} )

all: femlmtool.so

femlmtool.so: femlmtool.o
	$(LD)  $(LDFLAGS) -o $@ $< $(LDLIBS)

femlmtool.o: femlmtool.cc
	$(CXX) -g -c -O3 -fomit-frame-pointer \
	$(CPPFLAGS) \
	-o $@ \
	$<

clean:
	rm *.o femlmtool.so
