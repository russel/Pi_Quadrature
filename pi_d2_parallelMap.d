/*
 *  A D program to calculate Pi using quadrature as a parallel map algorithm.
 *
 *  Copyright © 2010--2011 Russel Winder
 */

//  std.parallelism is currently not in Phobos2, though it is being voted on for inclusion in Phobos2, so
//  ensure the compilation command takes care of all the factors to include the library.

import std.datetime ;
import std.parallelism ;
import std.stdio ;
import std.typecons ;

real partialSum ( immutable Tuple ! ( long , long , double ) data ) { 
  auto sum = 0.0 ;
  foreach ( i ; data[0] .. data[1] ) {
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
  auto inputData = new Tuple ! ( long , long , double ) [ numberOfTasks ] ;
  //
  //  The D compiler cannot currently (2.052) handle tuples with elements of immutable type.  So without the cast, the following
  //  error message is emitted:
  //
  //      Error: template instance std.typecons.tuple!(int,int,immutable(double)) error instantiating
  //
  //foreach ( i ; 0 .. numberOfTasks ) { inputData[i] = tuple ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ; }
  foreach ( i ; 0 .. numberOfTasks ) { inputData[i] = tuple ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , cast ( double ) ( delta ) ) ; }
  //
  //  Cannot have outputData be immutable as this results in the compiler saying:
  //
  //    Error: cannot implicitly convert expression (map(inputData)) of type Map!(partialSum,Tuple!(long,long,double)[]) to immutable(Map!(partialSum,Tuple!(long,long,double)[]))
  //
  //  David Simcha reports that using explicit TaskPool creation is only for special cases, that using the
  //  lazy, singleton taskPool is the right way of handling this sort of map use.
  //
  //  There is a known problem with core.cpuid (http://d.puremagic.com/issues/show_bug.cgi?id=5612) that
  //  means that the detection of the number of cores doesn't work properly.  It seems more problematic in
  //  64-bit working than 32-bit working.  Fortunately the number of threads in the pool can be forced by
  //  statement such as the following.  
  //
  defaultPoolThreads = 8 ;
  //
  auto outputData = taskPool.map ! ( partialSum ) ( inputData ) ;
  immutable pi = 4.0 * taskPool.reduce ! ( "a + b" ) ( 0.0 , outputData ) * delta ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Parallel Map pi = %.18f" , pi ) ;
  writefln ( "==== D Parallel Map iteration count = %d" , n ) ;
  writefln ( "==== D Parallel Map elapse = %f" , elapseTime ) ;
  writefln ( "==== D Parallel Map task count = %d" , numberOfTasks ) ;
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
