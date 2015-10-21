#!/bin/bash

SST_VERSION=${shell sst --version | sed -e 's/^[^(]*(\([^,]*\),.*/\1/g' }
SST_PKGCONFIG="SST-${SST_VERSION}"

ifeq ("${INTEL_PIN_DIRECTORY}", "")
PIN_ROOT = ${HOME}/pin/2.14-gcc/
else
PIN_ROOT = ${INTEL_PIN_DIRECTORY}
endif
CONFIG_ROOT = $(PIN_ROOT)/source/tools/Config

KIT=1
include $(CONFIG_ROOT)/unix.vars
# Bug in some versions of pin.  Fix
TARGET_OS = $(HOST_OS)
include $(CONFIG_ROOT)/makefile.unix.config



ifeq ($(TARGET_OS),linux)
CXX      = g++
SHEXT    = so
endif

ifeq ($(TARGET_OS),mac)
CXX      = c++
SHEXT    = dylib
endif

LD        = $(CXX)
CXXFLAGS += $(TOOL_CXXFLAGS) $(shell pkg-config --cflags ${SST_PKGCONFIG} )
LDFLAGS  = $(TOOL_LDFLAGS) $(TOOL_LPATHS)
LDLIBS = $(DL_LIB) $(TOOL_LIBS)

all: fesimple.$(SHEXT) fesimple_r.$(SHEXT)

fesimple_r.$(SHEXT): fesimple_r.o
	$(LD)  $(LDFLAGS) -o $@ $< $(LDLIBS) -lpthread -lrt

fesimple.$(SHEXT): fesimple.o
	$(LD)  $(LDFLAGS) -o $@ $< $(LDLIBS) -lpthread -lrt

%.o: %.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) -c -o $@ $<

clean:
	$(RM) -f *.o fesimple.$(SHEXT)
