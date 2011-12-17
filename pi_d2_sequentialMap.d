/*
 *  A D program to calculate Pi using quadrature as a sequential map algorithm.  This is really just here as
 *  a comparison against the parallel map version.
 *
 *  Copyright © 2010–2011 Russel Winder
 */

import std.algorithm ;
import std.datetime ;
import std.range ;
import std.stdio ;
import std.typecons ;

real partialSum ( immutable Tuple ! ( int , int , double ) data ) { 
  immutable start = 1 + data[0] * data[1] ;
  immutable end = ( data[0] + 1 ) * data[1] ;
  auto sum = 0.0 ;
  foreach ( i ; start .. end ) {
    immutable x = ( i - 0.5 ) * data[2] ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  return sum ;
}

void execute ( immutable int numberOfTasks ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  immutable sliceSize = n / numberOfTasks ;
  auto inputData = new Tuple ! ( int , int , double ) [ numberOfTasks ] ;
  //
  //  The D compiler cannot currently (2.052) handle tuples with elements of immutable type.  So without the
  //  cast, the following error message is emitted:
  //
  //      Error: template instance std.typecons.tuple!(int,immutable(int),immutable(double)) error instantiating
  //
  //foreach ( i ; 0 .. numberOfTasks ) { inputData[i] = tuple ( i , sliceSize , delta ) ; }
  foreach ( i ; 0 .. numberOfTasks ) { inputData[i] = tuple ( i , cast ( int ) ( sliceSize ) , cast ( double ) ( delta ) ) ; }
  //
  //  Should replace the above two with the following but it causes a compilation error.
  //
  //auto inputData = map ! ( ( i ) { return tuple ( i , cast ( int ) ( sliceSize ) , cast ( double ) ( delta ) ) ; } ) ( iota ( numberOfTasks ) ) ;
  immutable pi = 4.0 * delta * reduce ! ( ( a , b ) { return a + b ; } ) ( 0.0 , map ! ( partialSum ) ( inputData ) ) ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Sequential Map pi = %.18f" , pi ) ;
  writefln ( "==== D Sequential Map iteration count = %d" , n ) ;
  writefln ( "==== D Sequential Map elapse = %f" , elapseTime ) ;
  writefln ( "==== D Sequential Map task count = %d" , numberOfTasks ) ;
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
