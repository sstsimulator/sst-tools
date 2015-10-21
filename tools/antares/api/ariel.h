// Copyright 2009-2015 Sandia Corporation. Under the terms
// of Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
// Government retains certain rights in this software.
//
// Copyright (c) 2009-2015, Sandia Corporation
// All rights reserved.
//
// This file is part of the SST software package. For license
// information, see the LICENSE file in the top level directory of the
// distribution.


#ifndef _H_ARIEL_API
#define _H_ARIEL_API

#include <stdint.h>

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

void ariel_enable();
uint64_t ariel_cycles();

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
