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


#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef int mlm_Tag;

void* mlm_malloc(size_t size, int level);
void  mlm_free(void* ptr);
mlm_Tag mlm_memcpy(void* dest, void* src, size_t length);
void mlm_waitComplete(mlm_Tag in);
void mlm_set_pool(int pool);

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
