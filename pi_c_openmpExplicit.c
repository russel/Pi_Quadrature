/*
 *  A C program to calculate Pi using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

#include <stdio.h>
#include <omp.h>
#include "microsecondTime.h"

void execute ( const int numberOfThreads ) {
  const int n = 1000000000 ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const int sliceSize =  n / numberOfThreads ;
  double sum = 0.0 ;
#pragma omp parallel for reduction ( + : sum )
  for ( int i = 0 ; i < numberOfThreads ; ++i ) {
    const int start = 1 + i * sliceSize ;
    const int end = ( i + 1 ) * sliceSize ;
    for ( int j = start ; j <= end ; ++j ) {
      const double x = ( j - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
  }
  const double pi = 4.0 * delta * sum ;
  const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  printf ( "==== C OpenMP Explicit pi = %.18lf\n" , pi ) ;
  printf ( "==== C OpenMP Explicit iteration count = %d\n" ,  n ) ;
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
