/*
 *  A C program to calculate Pi using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2008-10 Russel Winder
 */

#include <stdio.h>
#include <omp.h>
#include "microsecondTime.h"

void execute ( const int numberOfThreads ) {
  const long n = 1000000000l ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const long sliceSize =  n / numberOfThreads ;
  double sum = 0.0 ;
  int i ;
#pragma omp parallel for private ( i ) reduction ( + : sum )
  for ( i = 0 ; i < numberOfThreads ; ++i ) {
    const long start = 1 + i * sliceSize ;
    const long end = ( i + 1 ) * sliceSize ;
    long j ;
    for ( j = start ; j <= end ; ++j ) {
      const double x = ( j - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
  }
  const double pi = 4.0 * sum * delta ;
  const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  printf ( "==== C OpenMP Explicit pi = %.18lf\n" , pi ) ;
  printf ( "==== C OpenMP Explicit iteration count = %ld\n" ,  n ) ;
  printf ( "==== C OpenMP Explicit elapse = %lf\n" , elapseTime ) ;
  printf ( "==== C OpenMP Explicit threadCount = %d\n" , numberOfThreads ) ;
  printf ( "==== C OpenMP Explicit processor count = %d\n" , omp_get_num_procs ( ) ) ;
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
