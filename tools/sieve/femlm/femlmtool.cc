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

#include <execinfo.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include "pin.H"
#include <time.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <iostream>
#include <fstream>
#include <map>
#include <stack>
#include <ctime>
#include <bitset>

#ifdef ARIEL_MLM_USE_MALLOC_OVERRIDE
#include <malloc.h>
#endif

#include <sst/elements/ariel/ariel_shmem.h>
using namespace SST::ArielComponent;

KNOB<string> SSTNamedPipe(KNOB_MODE_WRITEONCE, "pintool",
    "p", "", "Named pipe to connect to SST simulator");
KNOB<UINT64> MaxInstructions(KNOB_MODE_WRITEONCE, "pintool",
    "i", "10000000000", "Maximum number of instructions to run");
KNOB<UINT32> SSTVerbosity(KNOB_MODE_WRITEONCE, "pintool",
    "v", "0", "SST verbosity level");
KNOB<UINT32> MaxCoreCount(KNOB_MODE_WRITEONCE, "pintool",
    "c", "1", "Maximum core count to use for data pipes.");
KNOB<UINT32> StartupMode(KNOB_MODE_WRITEONCE, "pintool",
    "s", "1", "Mode for configuring profile behavior, 1 = start enabled, 0 = start disabled");
KNOB<UINT32> InterceptMultiLevelMemory(KNOB_MODE_WRITEONCE, "pintool",
    "m", "1", "Should intercept multi-level memory allocations, copies and frees, 1 = start enabled, 0 = start disabled");
KNOB<UINT32> DefaultMemoryPool(KNOB_MODE_WRITEONCE, "pintool",
    "d", "0", "Default SST Memory Pool");

//PIN_LOCK pipe_lock;
UINT32 core_count;
UINT32 default_pool;
ArielTunnel *tunnel = NULL;
bool enable_output;
PIN_LOCK mainLock;
std::vector<void*> allocated_list;

UINT32 overridePool;
bool shouldOverride;

VOID Fini(INT32 code, VOID *v)
{
	if(SSTVerbosity.Value() > 0) {
		std::cout << "SSTARIEL: Execution completed, shutting down." << std::endl;
	}

    ArielCommand ac;
    ac.command = ARIEL_PERFORM_EXIT;
    tunnel->writeMessage(0, ac);

    delete tunnel;
}

VOID copy(void* dest, const void* input, UINT32 length) {
	for(UINT32 i = 0; i < length; ++i) {
		((char*) dest)[i] = ((char*) input)[i];
	}
}

VOID WriteInstructionRead(ADDRINT* address, UINT32 readSize, THREADID thr) {
	if(enable_output) {
		uint64_t addr64 = (uint64_t) address;
		uint32_t thrID = (uint32_t) thr;

		assert(thr < core_count);

        ArielCommand ac;
        ac.command = ARIEL_PERFORM_READ;
        ac.inst.addr = addr64;
        ac.inst.size = readSize;
        tunnel->writeMessage(thr, ac);
	}
}

VOID WriteInstructionWrite(ADDRINT* address, UINT32 writeSize, THREADID thr) {
	if(enable_output) {
		uint64_t addr64 = (uint64_t) address;
		uint32_t thrID = (uint32_t) thr;

        ArielCommand ac;
        ac.command = ARIEL_PERFORM_WRITE;
        ac.inst.addr = addr64;
        ac.inst.size = writeSize;
        tunnel->writeMessage(thr, ac);
	}
}

VOID WriteStartInstructionMarker(UINT32 thr) {
    ArielCommand ac;
    ac.command = ARIEL_START_INSTRUCTION;
    tunnel->writeMessage(thr, ac);
}

VOID WriteEndInstructionMarker(UINT32 thr) {
    ArielCommand ac;
    ac.command = ARIEL_END_INSTRUCTION;
    tunnel->writeMessage(thr, ac);
}

VOID WriteInstructionReadWrite(THREADID thr, ADDRINT* readAddr, UINT32 readSize,
	ADDRINT* writeAddr, UINT32 writeSize) {

	if(enable_output) {
		if(thr < core_count) {

            const uint64_t wAddr64 = (uint64_t) writeAddr;
            const uint32_t wSize   = (uint32_t) writeSize;
            const uint64_t rAddr64 = (uint64_t) readAddr;
            const uint32_t rSize   = (uint32_t) readSize;

			const uint32_t thrID = (uint32_t) thr;

            ArielCommand acStart, acRead, acWrite, acEnd;

            acStart.command = ARIEL_START_INSTRUCTION;
            tunnel->writeMessage(thrID, acStart);
            acRead.command = ARIEL_PERFORM_READ;
            acRead.inst.addr = rAddr64;
            acRead.inst.size = rSize;
            tunnel->writeMessage(thrID, acRead);
            acWrite.command = ARIEL_PERFORM_WRITE;
            acWrite.inst.addr = wAddr64;
            acWrite.inst.size = wSize;
            tunnel->writeMessage(thrID, acWrite);
            acEnd.command = ARIEL_END_INSTRUCTION;
            tunnel->writeMessage(thrID, acEnd);
		}
	}

}

VOID WriteInstructionReadOnly(THREADID thr, ADDRINT* readAddr, UINT32 readSize) {

	if(enable_output) {
		if(thr < core_count) {

            const uint64_t rAddr64 = (uint64_t) readAddr;
            const uint32_t rSize   = (uint32_t) readSize;

            const uint32_t thrID = (uint32_t) thr;

            ArielCommand acStart, acRead, acEnd;
            acStart.command = ARIEL_START_INSTRUCTION;
            tunnel->writeMessage(thrID, acStart);
            acRead.command = ARIEL_PERFORM_READ;
            acRead.inst.addr = rAddr64;
            acRead.inst.size = rSize;
            tunnel->writeMessage(thrID, acRead);
            acEnd.command = ARIEL_END_INSTRUCTION;
            tunnel->writeMessage(thrID, acEnd);
		}
	}

}

VOID WriteNoOp(THREADID thr) {
	if(enable_output) {
		if(thr < core_count) {
            ArielCommand ac;
            ac.command = ARIEL_NOOP;
            tunnel->writeMessage(thr, ac);
		}
	}
}

VOID WriteInstructionWriteOnly(THREADID thr, ADDRINT* writeAddr, UINT32 writeSize) {

	if(enable_output) {
		if(thr < core_count) {

            const uint64_t wAddr64 = (uint64_t) writeAddr;
            const uint32_t wSize   = (uint32_t) writeSize;

            const uint32_t thrID = (uint32_t) thr;

            ArielCommand acStart, acWrite, acEnd;
            acStart.command = ARIEL_START_INSTRUCTION;
            tunnel->writeMessage(thrID, acStart);
            acWrite.command = ARIEL_PERFORM_WRITE;
            acWrite.inst.addr = wAddr64;
            acWrite.inst.size = wSize;
            tunnel->writeMessage(thrID, acWrite);
            acEnd.command = ARIEL_END_INSTRUCTION;
            tunnel->writeMessage(thrID, acEnd);
		}
	}

}

VOID InstrumentInstruction(INS ins, VOID *v)
{
	if( INS_IsMemoryRead(ins) && INS_IsMemoryWrite(ins) ) {
		INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR)
			WriteInstructionReadWrite,
			IARG_THREAD_ID,
			IARG_MEMORYREAD_EA, IARG_UINT32, INS_MemoryReadSize(ins),
			IARG_MEMORYWRITE_EA, IARG_UINT32, INS_MemoryWriteSize(ins),
			IARG_END);
	} else if( INS_IsMemoryRead(ins) ) {
		INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR)
			WriteInstructionReadOnly,
			IARG_THREAD_ID,
			IARG_MEMORYREAD_EA, IARG_UINT32, INS_MemoryReadSize(ins),
			IARG_END);
	} else if( INS_IsMemoryWrite(ins) ) {
		INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR)
			WriteInstructionWriteOnly,
			IARG_THREAD_ID,
			IARG_MEMORYWRITE_EA, IARG_UINT32, INS_MemoryWriteSize(ins),
			IARG_END);
	} else {
		INS_InsertPredicatedCall(ins, IPOINT_BEFORE, (AFUNPTR)
			WriteNoOp,
			IARG_THREAD_ID,
			IARG_END);
	}

}

int ariel_mlm_memcpy(void* dest, void* source, size_t size) {
#ifdef ARIEL_DEBUG
	fprintf(stderr, "Perform a mlm_memcpy from Ariel from %p to %p length %llu\n",
		source, dest, size);
#endif

	char* dest_c = (char*) dest;
	char* src_c  = (char*) source;

	// Perform the memory copy on behalf of the application
	for(size_t i = 0; i < size; ++i) {
		dest_c[i] = src_c[i];
	}

	THREADID currentThread = PIN_ThreadId();
	UINT32 thr = (UINT32) currentThread;

	if(thr >= core_count) {
		fprintf(stderr, "Thread ID: %lu is greater than core count.\n", thr);
		exit(-4);
	}

    const uint64_t ariel_src     = (uint64_t) source;
    const uint64_t ariel_dest    = (uint64_t) dest;
    const uint32_t length        = (uint32_t) size;

    ArielCommand ac;
    ac.command = ARIEL_START_DMA;
    ac.dma_start.src = ariel_src;
    ac.dma_start.dest = ariel_dest;
    ac.dma_start.len = length;

    tunnel->writeMessage(thr, ac);

#ifdef ARIEL_DEBUG
	fprintf(stderr, "Done with ariel memcpy.\n");
#endif

	return 0;
}

void ariel_mlm_set_pool(int new_pool) {
#ifdef ARIEL_DEBUG
	fprintf(stderr, "Ariel perform a mlm_switch_pool to level %d\n", new_pool);
#endif

    THREADID currentThread = PIN_ThreadId();
    UINT32 thr = (UINT32) currentThread;

#ifdef ARIEL_DEBUG
    fprintf(stderr, "Requested: %llu, but expanded to: %llu (on thread: %lu) \n", size, real_req_size,
            thr);
#endif

    const uint32_t newDefaultPool = (uint32_t) new_pool;

    ArielCommand ac;
    ac.command = ARIEL_SWITCH_POOL;
    ac.switchPool.pool = newDefaultPool;
    tunnel->writeMessage(thr, ac);

	// Keep track of the default pool
	default_pool = (UINT32) new_pool;
}

void* ariel_mlm_malloc(size_t size, int level) {
	THREADID currentThread = PIN_ThreadId();
	UINT32 thr = (UINT32) currentThread;

#ifdef ARIEL_DEBUG
	fprintf(stderr, "%u, Perform a mlm_malloc from Ariel %zu, level %d\n", thr, size, level);
#endif
	if(0 == size) {
		fprintf(stderr, "YOU REQUESTED ZERO BYTES\n");
		void *bt_entries[64];
		int entry_returned = backtrace(bt_entries, 64);
		backtrace_symbols_fd(bt_entries, entry_returned, 1);
		exit(-8);
	}

	size_t page_diff = size % ((size_t)4096);
	size_t npages = size / ((size_t)4096);

    size_t real_req_size = 4096 * (npages + ((page_diff == 0) ? 0 : 1));

#ifdef ARIEL_DEBUG
	fprintf(stderr, "Requested: %llu, but expanded to: %llu (on thread: %lu) \n",
            size, real_req_size, thr);
#endif

	void* real_ptr = 0;
	posix_memalign(&real_ptr, 4096, real_req_size);

	const uint64_t virtualAddress = (uint64_t) real_ptr;
	const uint64_t allocationLength = (uint64_t) real_req_size;
	const uint32_t allocationLevel = (uint32_t) level;

    ArielCommand ac;
    ac.command = ARIEL_ISSUE_TLM_MAP;
    ac.mlm_map.vaddr = virtualAddress;
    ac.mlm_map.alloc_len = allocationLength;

    if(shouldOverride) {
       ac.mlm_map.alloc_level = overridePool;
    } else {
	ac.mlm_map.alloc_level = allocationLevel;
    }

    tunnel->writeMessage(thr, ac);

#ifdef ARIEL_DEBUG
	fprintf(stderr, "%u: Ariel mlm_malloc call allocates data at address: 0x%llx\n",
		thr, (uint64_t) real_ptr);
#endif

    PIN_GetLock(&mainLock, thr);
	allocated_list.push_back(real_ptr);
    PIN_ReleaseLock(&mainLock);
	return real_ptr;
}

void ariel_mlm_free(void* ptr) {
	THREADID currentThread = PIN_ThreadId();
	UINT32 thr = (UINT32) currentThread;

#ifdef ARIEL_DEBUG
	fprintf(stderr, "Perform a mlm_free from Ariel (pointer = %p) on thread %lu\n", ptr, thr);
#endif

	bool found = false;
	std::vector<void*>::iterator ptr_list_itr;
    PIN_GetLock(&mainLock, thr);
	for(ptr_list_itr = allocated_list.begin(); ptr_list_itr != allocated_list.end(); ptr_list_itr++) {
		if(*ptr_list_itr == ptr) {
			found = true;
			allocated_list.erase(ptr_list_itr);
			break;
		}
	}
    PIN_ReleaseLock(&mainLock);

	if(found) {
#ifdef ARIEL_DEBUG
		fprintf(stderr, "ARIEL: Matched call to free, passing to Ariel free routine.\n");
#endif
		free(ptr);

		const uint64_t virtAddr = (uint64_t) ptr;

        ArielCommand ac;
        ac.command = ARIEL_ISSUE_TLM_FREE;
        ac.mlm_free.vaddr = virtAddr;
        tunnel->writeMessage(thr, ac);

	} else {
		fprintf(stderr, "ARIEL: Call to free in Ariel did not find a matching local allocation, this memory will be leaked.\n");
	}

}

void mapped_ariel_enable() {
	fprintf(stderr, "ARIEL: Enabling memory and instruction tracing from program control.\n");
 	enable_output = true;
}

void* ariel_malloc_intercept(size_t size) {
	return ariel_mlm_malloc(size, default_pool);
}

void* ariel_calloc_intercept(size_t nmembers, size_t member_size) {
	const size_t total_bytes = nmembers * member_size;
	void* result = ariel_mlm_malloc(total_bytes, default_pool);
	char* result_char = (char*) result;

	// Zero out the memory, as required by the C spec.
	for(size_t i = 0; i < total_bytes; ++i) {
		result_char[i] = (char) 0;
	}

	return result;
}

int ariel_posix_memalign_intercept(void** ptr, size_t alignment, size_t size) {
	*ptr = ariel_mlm_malloc(size, default_pool);
}

void* ariel_memalign_intercept(size_t alignment, size_t size) {
	return ariel_mlm_malloc(size, default_pool);
}

void ariel_free_intercept(void* ptr) {
	ariel_mlm_free(ptr);
}

void* ariel_valloc_intercept(size_t size) {
	return ariel_mlm_malloc(size, default_pool);
}

void* ariel_pvalloc_intercept(size_t size) {
	return ariel_mlm_malloc(size, default_pool);
}

void* ariel_aligned_alloc_intercept(size_t alignment, size_t size) {
	return ariel_mlm_malloc(size, default_pool);
}

void* ariel_realloc_intercept(void* ptr, size_t size) {
	THREADID currentThread = PIN_ThreadId();
	UINT32 thr = (UINT32) currentThread;

	void* realloc_result = realloc(ptr, size);

	// A reallocation event occured, means memory was moved
	if(realloc_result != ptr) {
		std::vector<void*>::iterator ptr_list_itr;

        PIN_GetLock(&mainLock, thr);
		for(ptr_list_itr = allocated_list.begin(); ptr_list_itr != allocated_list.end(); ptr_list_itr++) {
			if(*ptr_list_itr == ptr) {
				allocated_list.erase(ptr_list_itr);
			}
		}
        PIN_ReleaseLock(&mainLock);
	}
}


int mapped_gettimeofday(struct timeval *tp, void *tzp) {
    if ( tp == NULL ) { errno = EINVAL ; return -1; }

    tunnel->getTime(tp);
    return 0;
}


VOID InstrumentRoutine(RTN rtn, VOID* args) {
//	if(SSTVerbosity.Value() > 0) {
//		fprintf(stderr, "ARIEL: Examining routine [%s] for instrumentation\n", RTN_Name(rtn).c_str());
//	}

/*	if(RTN_Name(rtn) == "malloc") {
		// We need to replace with something here
		std::cout << "Identified a malloc replacement function." << std::endl;
	} else if (RTN_Name(rtn) == "mlm_malloc") {
		// This means malloc far away.
		fprintf(stderr, "Identified routine: mlm_malloc, replacing with Ariel equivalent...\n");
		RTN_Replace(rtn, (AFUNPTR) ariel_mlm_malloc);
		fprintf(stderr, "Replacement complete.\n");
	} else if (RTN_Name(rtn) == "mlm_free") {
		fprintf(stderr, "Identified routine: mlm_free, replacing with Ariel equivalent...\n");
		RTN_Replace(rtn, (AFUNPTR) ariel_mlm_free);
		fprintf(stderr, "Replacement complete.\n");
	} else*/ 

	if (RTN_Name(rtn) == "ariel_enable") {
		fprintf(stderr,"Identified routine: ariel_enable, replacing with Ariel equivalent...\n");
		RTN_Replace(rtn, (AFUNPTR) mapped_ariel_enable);
		fprintf(stderr,"Replacement complete.\n");
    } else if (RTN_Name(rtn) == "gettimeofday") {
		fprintf(stderr,"Identified routine: gettimeofday, replacing with Ariel equivalent...\n");
		RTN_Replace(rtn, (AFUNPTR) mapped_gettimeofday);
		fprintf(stderr,"Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "mlm_malloc") {
        // This means we want a special malloc to be used (needs a TLB map inside the virtual core)
        fprintf(stderr,"Identified routine: mlm_malloc, replacing with Ariel equivalent...\n");
        AFUNPTR ret = RTN_Replace(rtn, (AFUNPTR) ariel_mlm_malloc);
        fprintf(stderr,"Replacement complete. (%p)\n", ret);
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "mlm_free") {
        fprintf(stderr,"Identified routine: mlm_free, replacing with Ariel equivalent...\n");
        RTN_Replace(rtn, (AFUNPTR) ariel_mlm_free);
        fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "mlm_set_pool") {
        fprintf(stderr, "Identifier routine: mlm_set_pool, replacing with Ariel equivalent...\n");
        RTN_Replace(rtn, (AFUNPTR) ariel_mlm_set_pool);
        fprintf(stderr, "Replacement complete.\n");
    }
/*
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "malloc") {
	fprintf(stderr, "Identifier routine: malloc, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_malloc_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "free") {
	fprintf(stderr, "Identifier routine: free, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_free_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "calloc") {
	fprintf(stderr, "Identifed routine: calloc, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_calloc_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "posix_memalign") {
	fprintf(stderr, "Identifed routine: posix_memalign, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_posix_memalign_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "memalign") {
	fprintf(stderr, "Identifed routine: memalign, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_memalign_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "realloc") {
	fprintf(stderr, "Identifed routine: realloc, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_realloc_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "valloc") {
	fprintf(stderr, "Identifed routine: valloc, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_valloc_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "pvalloc") {
	fprintf(stderr, "Identifed routine: pvalloc, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_pvalloc_intercept);
	fprintf(stderr, "Replacement complete.\n");
    } else if ((InterceptMultiLevelMemory.Value() > 0) && RTN_Name(rtn) == "aligned_alloc") {
	fprintf(stderr, "Identifed routine: aligned_alloc, replacing with an Ariel management function...\n");
	RTN_Replace(rtn, (AFUNPTR) ariel_aligned_alloc_intercept);
	fprintf(stderr, "Replacement complete.\n");
    }
*/
 /*else if (RTN_Name(rtn) == "mlm_memcpy" ) {
	//	fprintf(stderr, "Identified routine: mlm_memcpy, replacing with Ariel equivalent...\n");
	//	RTN_Replace(rtn, (AFUNPTR) ariel_mlm_memcpy);
	//	fprintf(stderr, "Replacement complete.\n");
	}*/
}

/* ===================================================================== */
/* Print Help Message                                                    */
/* ===================================================================== */

INT32 Usage()
{
    PIN_ERROR( "This Pintool collects statistics for instructions.\n" 
              + KNOB_BASE::StringKnobSummary() + "\n");
    return -1;
}

/* ===================================================================== */
/* Main                                                                  */
/* ===================================================================== */

int main(int argc, char *argv[])
{
    if (PIN_Init(argc, argv)) return Usage();

    // Load the symbols ready for us to mangle functions.
    PIN_InitSymbols();
    PIN_AddFiniFunction(Fini, 0);

    if(SSTVerbosity.Value() > 0) {
	std::cout << "SSTARIEL: Loading Ariel Tool to connect to SST on pipe: " <<
		SSTNamedPipe.Value() << " max instruction count: " <<
		MaxInstructions.Value() <<
		" max core count: " << MaxCoreCount.Value() << std::endl;
    }

#ifdef ARIEL_MLM_USE_MALLOC_OVERRIDE
    if(InterceptMultiLevelMemory.Value() > 0) {
	mallopt(M_CHECK_ACTION, (int) 1);
    }
#endif

    char* override_pool_name = getenv("ARIEL_OVERRIDE_POOL");
    if(NULL != override_pool_name) {
	fprintf(stderr, "ARIEL-SST: Override for memory pools\n");
	shouldOverride = true;
	overridePool = (UINT32) atoi(getenv("ARIEL_OVERRIDE_POOL"));
	fprintf(stderr, "ARIEL-SST: Use pool: %lu instead of application provided\n", overridePool);
    } else {
	fprintf(stderr, "ARIEL-SST: Did not find ARIEL_OVERRIDE_POOL in the environment, no override applies.\n");
    }

    core_count = MaxCoreCount.Value();

    tunnel = new ArielTunnel(SSTNamedPipe.Value());

	fprintf(stderr, "ARIEL-SST PIN tool activating with %lu threads\n", core_count);
	fflush(stdout);

    sleep(1);

    default_pool = DefaultMemoryPool.Value();
    fprintf(stderr, "ARIEL: Default memory pool set to %lu\n", default_pool);

    if(StartupMode.Value() == 1) {
	fprintf(stderr, "ARIEL: Tool is configured to begin with profiling immediately.\n");
	enable_output = true;
    } else if (StartupMode.Value() == 0) {
	fprintf(stderr, "ARIEL: Tool is configured to suspend profiling until program control\n");
	enable_output = false;
    }

//    InitLock(&pipe_lock);
    INS_AddInstrumentFunction(InstrumentInstruction, 0);
    RTN_AddInstrumentFunction(InstrumentRoutine, 0);

    fprintf(stderr, "ARIEL: Starting program.\n");
    fflush(stdout);
    PIN_StartProgram();

    return 0;
}

