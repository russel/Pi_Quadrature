/*
 *  A D program to calculate Pi using quadrature as a parallel map algorithm.
 *
 *  Copyright © 2010 Russel Winder
 */

import std.algorithm ;
import std.date ;

//  Really ought to write something like:
//
//real partialSum ( const Tuple ! ( long , long , real ) data ) { 
//
//  but this fails to compile.  Instead it seems we have to do:

real partialSum ( T ... ) ( immutable T data ) {
  auto sum = 0.0L ;
  foreach ( i ; data[0] .. data[1] ) {
    immutable x = ( i - 0.5 ) * data[2] ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  return sum ;
}

void execute ( immutable int numberOfThreads ) {
  immutable n = 1000000000L ;
  immutable delta = 1.0 / n ;
  immutable startTime = getUTCtime ( ) ;
  immutable sliceSize = n / numberOfThreads ;
  auto inputData = Tuple ! ( long , long , real ) [ numberOfThreads ] ;
  foreach ( i ; 0 .. numberOfThreads ) { inputData[i] = tuple ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ; }
  auto outputData = map ! ( partialSum ) ( inputData ) ;
  immutable pi = 4.0 * reduce ! ( "a + b" ) ( 0 , outputData ) * delta ;
  immutable elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Parallel Map pi = %f" , pi ) ;
  writefln ( "==== D Parallel Map iteration count = %d" , n ) ;
  writefln ( "==== D Parallel Map elapse = %f" , elapseTime ) ;
  writefln ( "==== D Parallel Map thread count = %d" , numberOfThreads ) ;
}

int main ( immutable string[] args ) {
  execute ( 1 ) ;
  writeln ( ) ;
  execute ( 2 ) ;
  writeln ( ) ;
  execute ( 8 ) ;
  writeln ( ) ;
  execute ( 32 ) ;
  return 0 ;
}
