/*
 *  A C++ program to calculate Pi using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <omp.h>
#include "microsecondTime.h"

int main ( ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  auto sum = 0.0 ;
#pragma omp parallel for reduction ( + : sum )
  for ( auto i = 1 ; i <= n ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  const auto pi = 4.0 * sum * delta ;
  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ OpenMP Implicit pi = " << std::setprecision ( 18 ) << pi << std::endl ;
  std::cout << "==== C++ OpenMP Implicit iteration count = " << n << std::endl ;
  std::cout << "==== C++ OpenMP Implicit elapse = " << elapseTime << std::endl ;
  std::cout << "==== C++ OpenMP Implicit processor count = " << omp_get_num_procs ( ) << std::endl ;
  return 0 ;
}
