/*
 *  A D program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import std.date ;
import std.stdio ;

int main ( immutable string[] args ) {
  immutable n = 1000000000L ;
  immutable delta = 1.0 / n ;
  immutable startTime = getUTCtime ( ) ;
  auto sum = 0.0L ;
  foreach ( i ; 1 .. n ) {
    immutable x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  immutable pi = 4.0 * sum * delta ;
  immutable elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Sequential pi = %.18f" , pi ) ;
  writefln ( "==== D Sequential iteration count = %d" , n ) ;
  writefln ( "==== D Sequential elapse = %f" , elapseTime ) ;
  return 0 ;
}
