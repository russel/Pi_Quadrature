/*
 *  A D program to calculate Pi using quadrature as a parallel map algorithm.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

//  std.parallelism is currently not in Phobos2, though it is being voted on for inclusion in Phobos2, so
//  ensure the compilation command takes care of all the factors to include the library.

import std.algorithm ;
import std.datetime ;
import std.parallelism ;
import std.stdio ;
import std.typecons ;

real partialSum ( immutable Tuple ! ( int , int , double ) data ) { 
  immutable start = 1 + data[0] * data[1] ;
  immutable end = ( data[0] + 1 ) * data[1] ;
  auto sum = 0.0 ;
  foreach ( i ; start .. end + 1 ) {
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
  //  The D compiler cannot currently (2.052) handle tuples with elements of immutable type.  So without the cast, the following
  //  error message is emitted:
  //
  //      Error: template instance std.typecons.tuple!(int,int,immutable(double)) error instantiating
  //
  //foreach ( i ; 0 .. numberOfTasks ) { inputData[i] = tuple ( i , sliceSize , delta ) ; }
  foreach ( i ; 0 .. numberOfTasks ) { inputData[i] = tuple ( i ,  cast ( int ) ( sliceSize ) , cast ( double ) ( delta ) ) ; }
  //  Remember to use eager map not lazy map here!
  immutable pi = 4.0 * delta * reduce ! ( ( a , b ) { return a + b ; } ) ( 0.0 , taskPool.amap ! ( partialSum ) ( inputData ) ) ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Parallel Map Sequential Reduce pi = %.18f" , pi ) ;
  writefln ( "==== D Parallel Map Sequential Reduce iteration count = %d" , n ) ;
  writefln ( "==== D Parallel Map Sequential Reduce elapse = %f" , elapseTime ) ;
  writefln ( "==== D Parallel Map Sequential Reduce task count = %d" , numberOfTasks ) ;
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
