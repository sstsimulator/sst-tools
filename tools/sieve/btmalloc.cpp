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

// File:   btmalloc.cpp - back trace malloc

#include <stdio.h>
#include <dlfcn.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <unistd.h>
#include <execinfo.h>
#include <assert.h>
#include <errno.h>
#include <atomic>
#include <sys/mman.h>
#include <pthread.h>

static size_t cutoffSize = 8; // cutoff size in bytes

static void* (*sys_malloc)(size_t)=NULL;
static void* (*sys_calloc)(size_t, size_t)=NULL;
static void* (*sys_realloc)(void *, size_t)=NULL;
static void* (*sys_mmap)(void*, size_t, int, int, int, off_t)=NULL;
static void  (*sys_free)(void *)=NULL;
// static void* (*sys_memalign)(size_t, size_t)=NULL;
// static  int  (*sys_posix_memalign)(void**, size_t, size_t)=NULL;
// static void* (*sys_valloc)(size_t)=NULL;

static std::atomic<bool> isNested(false);
// It appears that the recursive pthread mutexes do not perform additional
// malloc. Therefore, use it.

class Mutex {
  pthread_mutex_t mtx;
  pthread_mutexattr_t attr;

public:
  Mutex () {
    // Initialize the recursive mutex
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init(&mtx, &attr);
  }

  void lock () {
    pthread_mutex_lock(&mtx);
  }

  void unlock () {
    pthread_mutex_unlock(&mtx);
  }
};

static Mutex mutex;

//static int lockVar = 1; // initialize the variable to true

// #define SPIN_LOCK(a) { while (! __sync_bool_compare_and_swap(&(a), 1, 0) ); }
// #define UNLOCK(a) { a = 1; }


static void mtrace_init(void)
{
  mutex.lock();
  sys_malloc   = (void*(*)(size_t)) dlsym(RTLD_NEXT, "malloc");
  sys_calloc   = (void*(*)(size_t, size_t)) dlsym(RTLD_NEXT, "calloc");
  sys_realloc  = (void*(*)(void*, size_t)) dlsym(RTLD_NEXT, "realloc");
  sys_mmap     = (void*(*)(void*, size_t, int, int, int, off_t)) dlsym(RTLD_NEXT, "mmap");
  sys_free   =  (void(*)(void *)) dlsym(RTLD_NEXT, "free");
  
  if (NULL == sys_malloc
      || NULL == sys_calloc
      || NULL == sys_realloc
      || NULL == sys_mmap
      || NULL == sys_free)
  {
     fprintf(stderr, "Error in `dlsym`: %s\n", dlerror());
     exit(0);
  }
  // Check if the environment specifies a cutoff
  char *cutoff = getenv("BTMALLOC_CUTOFF_SIZE");
  if (cutoff != NULL)  cutoffSize = atoi(cutoff);

  mutex.unlock();
}


class Btrace {
public:
  static const int size = 500; // maximum stack depth
  volatile std::atomic<bool> inited;
  
  void *buffer[size];
  int outFile;

  Btrace() {
    inited = false; // atomic store
    outFile = open("backTrace.txt", O_WRONLY | O_CREAT | O_TRUNC,
		   S_IRWXU | S_IRWXG ); // User and group have read/write/execute/search permissions
    assert(outFile > -1 && strerror(errno));
    mtrace_init();
    inited = true; // atomic store
  }
  
  ~Btrace() {
    close(outFile);
  }
};

static Btrace btrace;


char tmpbuf[1024];
unsigned long tmppos = 0;
unsigned long tmpallocs = 0;

inline void* dummy_malloc(size_t size) {
  mutex.lock();
  if (tmppos + size >= sizeof(tmpbuf)) exit(1);
  void *retptr = tmpbuf + tmppos;
  tmppos += size;
  ++tmpallocs;

  mutex.unlock();
  return retptr;
}

inline void* dummy_calloc(size_t nmemb, size_t size) {
  mutex.lock();
  void *ptr = dummy_malloc(nmemb * size);
  unsigned int i = 0;
  for (; i < nmemb * size; ++i) {
    *( (char*) ptr + i) = '\0';
  }
  
  mutex.unlock();
  return ptr;
}


// dlsym will try to free. Keep this dummy free
inline void dummy_free(void *ptr) {
}


inline void printTrace(void *ptr, size_t size) {
  char strbuf[128];

  sprintf(strbuf, "VA: %p, SIZE: %lu\n", ptr, size);
  write(btrace.outFile, strbuf, strlen(strbuf));
  int stackDepth = backtrace(btrace.buffer, btrace.size);
  for (int i = 0; i < stackDepth; ++i) {
    sprintf(strbuf, "%p\n", btrace.buffer[i]);
    write(btrace.outFile, strbuf, strlen(strbuf));
  }
  //backtrace_symbols_fd(btrace.buffer, stackDepth, btrace.outFile);
}

void printPthreadID() {
  pthread_t pt = pthread_self();
  unsigned char *ptc = (unsigned char*)(void*)(&pt);
  printf("pthread id = 0x");
  for (size_t i=0; i<sizeof(pt); i++) {
    printf("%02x", (unsigned)(ptc[i]));
  }
  printf("\n");
}


template<typename T, typename ...Args>
inline T commonAlloc(size_t size, T (*f)(Args ...args), Args... args) {
  mutex.lock();

  bool isNestedOnEntry = isNested;
  isNested = true;
  T ptr = f(args...); // The actual system alloc call
  // Do not print trace when already inside another higher level alloc call
  if (isNestedOnEntry) {
    mutex.unlock();
    return ptr;
  }

  //printPthreadID();
  //printf("allocated ptr = %p, size = %llu\n", ptr, size);
  if (size >= cutoffSize) printTrace(ptr, size);
  isNested = false;

  mutex.unlock();
  return ptr;
}


extern "C" void* malloc(size_t size)
{
  if (!btrace.inited)   return dummy_malloc(size);
  return commonAlloc(size, sys_malloc, size);
}

// dummy_free is necessary because dlsym will be calling free.
extern "C" void free(void *ptr) {
  if (!btrace.inited)   return dummy_free(ptr);
  sys_free(ptr);
}

extern "C" void* calloc(size_t num, size_t size) {
  if (!btrace.inited)   return dummy_calloc(num, size);
  return commonAlloc(num*size, sys_calloc, num, size);  
}


extern "C" void* realloc(void *ptr, size_t size) {
  assert(btrace.inited && "Calling realloc when the alloc initialization has not yet completed");
  return commonAlloc(size, sys_realloc, ptr, size);
}


extern "C" void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset) {
  assert(btrace.inited && "Calling mmap when the alloc initialization has not yet completed");
  return commonAlloc(length, sys_mmap, addr, length, prot, flags, fd, offset);
}


// Not implementing realloc because it is complicated. The "actual" frees still
// happen through the system frees. Therefore, there is no correctness issue.
// However, we may miss a few allocations because of reallocs. 
// void* realloc(void *ptr, size_t size);

void* operator new(size_t sz) {
  return malloc(sz);
}


// The size information may be passed to the global delete operator.
// Since we store the size ourselves, we ignore the size passed.
void operator delete(void *ptr) {
  free(ptr);
  return;
}

