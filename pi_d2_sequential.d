/*
 *  A D program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

import std.datetime ;
import std.stdio ;

int main ( immutable string[] args ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  auto sum = 0.0 ;
  foreach ( i ; 1 .. n ) {
    immutable x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  immutable pi = 4.0 * delta * sum ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Sequential pi = %.18f" , pi ) ;
  writefln ( "==== D Sequential iteration count = %d" , n ) ;
  writefln ( "==== D Sequential elapse = %f" , elapseTime ) ;
  return 0 ;
}
