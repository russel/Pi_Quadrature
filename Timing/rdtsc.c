/*
 *  A C function to extract the time stamp counter on an x86 processor.
 *
 *  Copyright © 2008 Russel Winder
 */

#include "rdtsc.h"

#if ! defined ( INLINE_RDTSC )

/*
 *  Note that we assume that: %edx:%eax is the return value; we can ignore the stack protocol; we get a
 *  return instruction even if we don't put one; and the compiler is happy to ignore non-void functions not
 *  having a return statement.
 */
uint64_t rdtsc ( ) {
  __asm__ __volatile__ (
                        "xorl %%eax, %%eax\n"
                        "cpuid\n"
                        "rdtsc\n"
                        :
                        :
                        : "%eax" ,  "%ebx",  "%ecx",  "%edx"
                        ) ;
}

#endif /* ! defined (  INLINE_RDTSC ) */
