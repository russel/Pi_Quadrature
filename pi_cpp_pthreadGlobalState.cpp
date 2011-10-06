/*
 *  A C program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <pthread.h>
#include "microsecondTime.h"

const auto n = 1000000000 ;
const auto delta = 1.0 / n ;

int sliceSize  ;

double sum ;
pthread_mutex_t sumMutex ;

/*
 *  On a 32-bit machine a void* and an int probably have the same size so we can simply pass a literal value
 *  and cast the type.  On a 64-bit machines things get a bit more complicated.  For the moment just use a
 *  long.  This code depends on GCC being used.
 */
#if defined ( __LP64__ )
typedef long integerFromVoidStar ;
#else
typedef int integerFromVoidStar ;
#endif

void * partialSum ( void *const arg  ) {
  const auto start = 1 + ( (integerFromVoidStar) arg ) * sliceSize ;
  const auto end = ( ( (integerFromVoidStar) arg ) + 1 ) * sliceSize ;
  auto localSum = 0.0 ;
  for ( auto i = start ; i <= end ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  pthread_mutex_lock ( &sumMutex ) ;
  sum += localSum ;
  pthread_mutex_unlock ( &sumMutex ) ;
  pthread_exit ( (void *) 0 ) ;
  return 0 ;
}

void execute ( const int numberOfThreads ) {
  const auto startTimeMicros = microsecondTime ( ) ;
  sum = 0.0 ; // Only one thread at this point so safe to access without locking.
  sliceSize = n / numberOfThreads ; // Only one thread at this point so safe to access without locking.
  pthread_mutex_init ( &sumMutex , NULL ) ;
  pthread_attr_t attributes ;
  pthread_attr_init ( &attributes ) ;
  pthread_attr_setdetachstate ( &attributes , PTHREAD_CREATE_JOINABLE ) ;
  pthread_t threads[numberOfThreads] ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { pthread_create ( &threads[i] , &attributes , partialSum , (void *) i ) ; }
  pthread_attr_destroy ( &attributes ) ;
  int status ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { pthread_join ( threads[i] , (void **) &status ) ; }
  const auto pi = 4.0 * sum * delta ;
  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ PThread global pi = " << std::setprecision ( 18 ) << pi << std::endl ;
  std::cout << "==== C++ PThread global iteration count = " << n << std::endl ;
  std::cout << "==== C++ PThread global elapse = " << elapseTime << std::endl ;
  std::cout << "==== C++ PThread global thread count = " <<  numberOfThreads << std::endl ;
}

int main ( ) {
  execute ( 1 ) ;
  std::cout << std::endl ;
  execute ( 2 ) ;
  std::cout << std::endl ;
  execute ( 8 ) ;
  std::cout << std::endl ;
  execute ( 32 ) ;
  return 0 ;
}
