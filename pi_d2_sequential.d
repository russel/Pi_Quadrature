/*
 *  A D program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009-11 Russel Winder
 */

import std.date ;
import std.stdio ;

//  As at version 2.050 D is a 32-bit system generating 32-bit code.  Using long rather than int makes this
//  quite a lot slower than the equivalents in C and C++.  64-bit D is due "very soon now".

int main ( immutable string[] args ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  immutable startTime = getUTCtime ( ) ;
  auto sum = 0.0 ;
  foreach ( i ; 1 .. n ) {
    immutable x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  immutable pi = 4.0 * sum * delta ;
  immutable elapseTime = ( cast ( double ) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Sequential pi = %.18f" , pi ) ;
  writefln ( "==== D Sequential iteration count = %d" , n ) ;
  writefln ( "==== D Sequential elapse = %f" , elapseTime ) ;
  return 0 ;
}
