/*
 *  A C function to extract the time stamp counter on an x86 processor.
 *
 *  Copyright © 2008 Russel Winder
 */

#if ! defined ( RDTSC_H )
#define RDTSC_H

#if defined ( __GNUC__ )

#include <stdint.h>

#if defined ( INLINE_RDTSC )

/*
 *  There is an article about rdtsc on Wikipedia (http://en.wikipedia.org/wiki/RDTSC).  It's GCC solution
 *  seems safe but slow.  The Visual C++ version appears a lot more efficient but note it does not do
 *  inlining, it has to be a real function.
 *
 *  The article recommends serializing the rdstc instruction by using the cpuid instruction.  %eax holds the
 *  parameter to the cpuid instruction, possible values are 0, 1 or 2.  cpuid fills in %eax, %ebx, %ecx,
 *  %edx.  rdtsc fills in %eax and %edx.
 *
 *   rdtscp is not a legal instruction despite what it says at http://en.wikipedia.org/wiki/RDTSC.
 */

__inline__ uint64_t rdtsc ( ) {
  uint32_t lo , hi ;
  __asm__ __volatile__ (
                        "xorl %%eax, %%eax\n"
                        "cpuid\n"
                        "rdtsc\n"
                        : "=a" ( lo ) , "=d" ( hi )
                        :
                        : "%ebx" ,  "%ecx"
                        ) ;
  return (uint64_t)hi << 32 | lo ;
}

#else /* defined ( INLINE_RDTSC ) */

/*
 *  We can get something seemingly very much more efficient by making the assumption that: %edx:%eax is the
 *  return value; we can ignore the stack protocol; we get a return instruction even if we don't put one;
 *  and the compiler is happy to ignore non-void functions not having a return statement.  However, this
 *  cannot be inlined.  So the question is whether the function call is more costly that the various move
 *  and xorl instructions.  Its a pity we cannot also make this naked.
 */

uint64_t rdtsc ( ) ;

#endif /* defined ( INLINE_RDTSC ) */

#endif /* defined ( __GNUC__ ) */

#endif /* ! defined ( RDTSC_H ) */
