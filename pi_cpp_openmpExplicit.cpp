/*
 *  A C program to calculate Pi using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <omp.h>
#include "microsecondTime.h"

void execute ( const int numberOfThreads ) {
  const long n = 1000000000l ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const long sliceSize =  n / numberOfThreads ;
  double sum = 0.0 ;
#pragma omp parallel for reduction ( + : sum )
  for ( int i = 0 ; i < numberOfThreads ; ++i ) {
    const long start = 1 + i * sliceSize ;
    const long end = ( i + 1 ) * sliceSize ;
    for ( long j = start ; j <= end ; ++j ) {
      const double x = ( j - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
  }
  const double pi = 4.0 * sum * delta ;
  const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ OpenMP Explicit pi = " << std::setprecision ( 18 ) << pi << std::endl ;
  std::cout << "==== C++ OpenMP Explicit iteration count = " << n << std::endl ;
  std::cout << "==== C++ OpenMP Explicit elapse = "  << elapseTime << std::endl ;
  std::cout << "==== C++ OpenMP Explicit threadCount = " <<  numberOfThreads << std::endl ;
  std::cout << "==== C++ OpenMP Explicit processor count = "  << omp_get_num_procs ( ) << std::endl ;
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
