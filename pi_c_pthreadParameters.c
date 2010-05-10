/*
 *  A C program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

#include <stdio.h>
#include <pthread.h>
#include "microsecondTime.h"

double sum ;
pthread_mutex_t sumMutex ;

typedef struct CalculationParameters {
  long id ;
  long sliceSize ;
  double delta ;
} CalculationParameters ;

void * partialSum ( void *const arg  ) {
  const long start = 1 + ( (CalculationParameters *const) arg )->id * ( (CalculationParameters *const) arg )->sliceSize ;
  const long end = ( ( (CalculationParameters *const) arg )->id + 1 ) * ( (CalculationParameters *const) arg )->sliceSize ;
  const double delta = ( (CalculationParameters *const) arg )->delta ;
  double localSum = 0.0 ;
  long i ;
  for ( i = start ; i <= end ; ++i ) {
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
  const long n = 1000000000l ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const long sliceSize  = n / numberOfThreads ;
  pthread_mutex_init ( &sumMutex , NULL ) ;
  pthread_attr_t attributes ;
  pthread_attr_init ( &attributes ) ;
  pthread_attr_setdetachstate ( &attributes , PTHREAD_CREATE_JOINABLE ) ;
  sum = 0.0 ; // Only one thread at this point so safe to access without locking.
  pthread_t threads[numberOfThreads] ;
  CalculationParameters parameters[numberOfThreads] ;
  int i ;
  for ( i = 0 ; i < numberOfThreads ; ++i ) {
    parameters[i].id = i ;
    parameters[i].sliceSize = sliceSize ;
    parameters[i].delta = delta ;
    pthread_create ( &threads[i] , &attributes , partialSum , (void *) &parameters[i] ) ;
  }
  pthread_attr_destroy ( &attributes ) ;
  int status ;
  for ( i = 0 ; i < numberOfThreads ; ++i ) { pthread_join ( threads[i] , (void **) &status ) ; }
  const double pi = 4.0 * sum * delta ;
  const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  printf ( "==== C PThread parameters pi = %.18lf\n" , pi ) ;
  printf ( "==== C PThread parameters iteration count = %ld\n" ,  n ) ;
  printf ( "==== C PThread parameters elapse = %lf\n" , elapseTime ) ;
  printf ( "==== C PThread parameters thread count = %d\n" , numberOfThreads ) ;
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
