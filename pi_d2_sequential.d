/*
 *  A D program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import std.date ;
import std.stdio ;

int main ( string[] args ) {
  invariant n = 1000000000L ;  //  Using int here instead of long would make this 40% faster but C and C++ use long.
  invariant delta = 1.0 / n ;  //  Using 1.0L makes things twice as slow if sum is real rather than double.
  invariant startTime = getUTCtime ( ) ;
  auto sum = 0.0L ;  //  Using a real here rather than a double makes things twice as fast!!!!!
  foreach ( i ; 1 .. n ) {
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
