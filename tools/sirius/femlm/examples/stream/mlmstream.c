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

#include <stdio.h>
#include <stdlib.h>
#include <mlm.h>

int main(int argc, char* argv[]) {

	const int LENGTH = 32768;

	printf("Allocating arrays of size %d elements.\n", LENGTH);
	double* a = (double*) mlm_malloc(sizeof(double) * LENGTH, 0);
	double* b = (double*) mlm_malloc(sizeof(double) * LENGTH, 0);
	double* fast_c = (double*) mlm_malloc(sizeof(double) * LENGTH, 0);

	mlm_set_pool(1);

	printf("Allocation for fast_c is %llu\n", (unsigned long long int) fast_c);
	double* c = (double*) malloc(sizeof(double) * LENGTH);
	printf("Done allocating arrays.\n");

	int i;
	for(i = 0; i < LENGTH; ++i) {
		a[i] = i;
		b[i] = LENGTH - i;
		c[i] = 0;
	}

	// Issue a memory copy
	mlm_memcpy(fast_c, c, sizeof(double) * LENGTH);

	printf("Perfoming the fast_c compute loop...\n");
	#pragma omp parallel for
	for(i = 0; i < LENGTH; ++i) {
		//printf("issuing a write to: %llu (fast_c)\n", ((unsigned long long int) &fast_c[i]));
		fast_c[i] = 2.0 * a[i] + 1.5 * b[i];
	}

	// Now copy results back
	mlm_Tag copy_tag = mlm_memcpy(c, fast_c, sizeof(double) * LENGTH);
        mlm_waitComplete(copy_tag);

	double sum = 0;
	for(i = 0; i < LENGTH; ++i) {
		sum += c[i];
	}

	printf("Sum of arrays is: %f\n", sum);
	printf("Freeing arrays...\n");

	mlm_free(a);
	mlm_free(b);
	free(c);

	printf("Done.\n");
}
