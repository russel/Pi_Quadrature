/*
 *  A C++ program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2008-10 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include "microsecondTime.h"

int main ( ) {
  const long n = 1000000000l ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  double sum = 0.0 ;
  for ( long i = 1 ; i <= n ; ++i ) {
    const double x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  const double pi = 4.0 * sum * delta ;
  const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Sequential pi = " << std::setprecision ( 18 ) << pi << std::endl ;
  std::cout << "==== C++ Sequential iteration count = " << n << std::endl ;
  std::cout << "==== C++ Sequential elapse = " << elapseTime << std::endl ;
  return 0 ;
}
