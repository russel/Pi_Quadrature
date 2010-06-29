/*
 *  A C program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2008-10 Russel Winder
 */

#include <stdio.h>
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
  printf ( "==== C Sequential pi = %.18lf\n" , pi ) ;
  printf ( "==== C Sequential iteration count = %ld\n" ,  n ) ;
  printf ( "==== C Sequential elapse = %lf\n" , elapseTime ) ;
  return 0 ;
}
