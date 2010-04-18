/*
 *  A D program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009 Russel Winder
 */

import std.stdio ;
import std.date ;

int main ( string[] args ) {
  invariant n = 1000000000L ;
  invariant delta = 1.0 / n ;
  invariant startTime = getUTCtime ( ) ;
  real sum = 0.0 ;
  for ( auto i = 1L ; i <= n ; ++i ) {
    invariant x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  invariant pi = 4.0 * sum * delta ;
  invariant elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Sequential pi = %.18f" , pi ) ;
  writefln ( "==== D Sequential iteration count = %d" , n ) ;
  writefln ( "==== D Sequential elapse = %f" , elapseTime ) ;
  return 0 ;
}
