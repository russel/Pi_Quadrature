/*
 *  A D program to calculate Pi using quadrature as a parallel map algorithm.
 *
 *  Copyright © 2011 Russel Winder
 */

//  std.parallelism is currently not in Phobos2, though it is being voted on for inclusion in Phobos2, so
//  ensure the compilation command takes care of all the factors to include the library.

//  This version is due to David Simcha, stemming from various emails ont he various D email lists and
//  reified in the documentation for std.parallelism,
//  http://cis.jhu.edu/~dsimcha/d/phobos/std_parallelism.html#reduce

import std.algorithm ;
import std.datetime ;
import std.parallelism ;
import std.range ;
import std.stdio ;

immutable n = 1000000000 ;
immutable delta = 1.0 / n ;

real getTerm ( int i ) {
  immutable x = ( i - 0.5 ) * delta ;
  return 1.0 / ( 1.0 + x * x ) ;
}

int main ( immutable string[] args ) {
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  immutable pi = 4.0 * taskPool.reduce ! ( "a + b") ( map ! getTerm ( iota ( n ) ) ) * delta ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Parallel Reduce DS pi = %.18f" , pi ) ;
  writefln ( "==== D Parallel Reduce DS iteration count = %d" , n ) ;
  writefln ( "==== D Parallel Reduce DS elapse = %f" , elapseTime ) ;
  //writefln ( "==== D Parallel ReduceDS task count = %d" , numberOfTasks ) ;
  return 0 ;
}
