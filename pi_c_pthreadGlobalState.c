/*
 *  A C program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

#include <stdio.h>
#include <pthread.h>
#include "microsecondTime.h"

#define n 1000000000
const double delta = 1.0 / n ;

int sliceSize  ;

double sum ;
pthread_mutex_t sumMutex ;

void * partialSum ( void *const arg  ) {
  const int start = 1 + ( (long) arg ) * sliceSize ;
  const int end = ( ( (long) arg ) + 1 ) * sliceSize ;
  double localSum = 0.0 ;
  for ( int i = start ; i <= end ; ++i ) {
    const double x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  pthread_mutex_lock ( &sumMutex ) ;
  sum += localSum ;
  pthread_mutex_unlock ( &sumMutex ) ;
  pthread_exit ( (void *) 0 ) ;
  return 0 ;
}

void execute ( const int numberOfThreads ) {
  const long long startTimeMicros = microsecondTime ( ) ;
  sum = 0.0 ; // Only one thread at this point so safe to access without locking.
  sliceSize = n / numberOfThreads ; // Only one thread at this point so safe to access without locking.
  pthread_mutex_init ( &sumMutex , NULL ) ;
  pthread_attr_t attributes ;
  pthread_attr_init ( &attributes ) ;
  pthread_attr_setdetachstate ( &attributes , PTHREAD_CREATE_JOINABLE ) ;
  pthread_t threads[numberOfThreads] ;
  for ( int i = 0 ; i < numberOfThreads ; ++i ) { pthread_create ( &threads[i] , &attributes , partialSum , (void *) i ) ; }
  pthread_attr_destroy ( &attributes ) ;
  int status ;
  for ( int i = 0 ; i < numberOfThreads ; ++i ) { pthread_join ( threads[i] , (void **) &status ) ; }
  const double pi = 4.0 * sum * delta ;
  const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  printf ( "==== C PThread global pi = %.18lf\n" , pi ) ;
  printf ( "==== C PThread global iteration count = %d\n" ,  n ) ;
  printf ( "==== C PThread global elapse = %lf\n" , elapseTime ) ;
  printf ( "==== C PThread global thread count = %d\n" , numberOfThreads ) ;
}

int main ( ) {
  execute ( 1 ) ;
  printf ( "\n" ) ;
  execute ( 2 ) ;
  printf ( "\n" ) ;
  execute ( 8 ) ;
  printf ( "\n" ) ;
  execute ( 32 ) ;
  return 0 ;
}
