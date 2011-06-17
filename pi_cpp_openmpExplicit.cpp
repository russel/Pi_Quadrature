/*
 *  A C program to calculate Pi using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <omp.h>
#include "microsecondTime.h"

void execute ( const int numberOfThreads ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize =  n / numberOfThreads ;
  auto sum = 0.0 ;
#pragma omp parallel for reduction ( + : sum )
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) {
    const auto start = 1 + i * sliceSize ;
    const auto end = ( i + 1 ) * sliceSize ;
    for ( auto j = start ; j <= end ; ++j ) {
      const auto x = ( j - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
  }
  const auto pi = 4.0 * sum * delta ;
  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
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
