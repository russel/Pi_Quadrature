/*
 *  A C++ program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2008-9 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include "microsecondTime.h"

int main ( ) {
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  long double sum = 0.0 ;
  for ( long i = 1 ; i <= n ; ++i ) {
    const long double x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  const long double pi = 4.0 * sum * delta ;
  const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Sequential pi = " << std::setprecision ( 25 ) << pi << std::endl ;
  std::cout << "==== C++ Sequential iteration count = " << n << std::endl ;
  std::cout << "==== C++ Sequential elapse = " << elapseTime << std::endl ;
  return 0 ;
}
