/*
 *  A D program to calculate Pi using quadrature as a parallel map algorithm.
 *
 *  Copyright © 2011 Russel Winder
 */

//  std.parallelism is currently not in Phobos2, though it is being voted on for inclusion in Phobos2, so
//  ensure the compilation command takes care of all the factors to include the library.

//  This version is due to David Simcha, stemming from various emails on the various D email lists and
//  reified in the documentation for std.parallelism,
//  http://cis.jhu.edu/~dsimcha/d/phobos/std_parallelism.html#reduce

import std.algorithm ;
import std.datetime ;
import std.parallelism ;
import std.range ;
import std.stdio ;

immutable n = 1000000000 ;
immutable delta = 1.0 / n ;

//  The function used in the reduce must be global.  Attempting to use a nested function results in a
//  compilation failure.  Thus the value delta is above and global, but that is fine as it is immutable.

real getTerm ( int i ) {
  immutable x = ( i - 0.5 ) * delta ;
  return 1.0 / ( 1.0 + x * x ) ;
}

int main ( immutable string[] args ) {
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  //
  //  There is a problem using a lambda function here.  David Simcha reports it is a consequence of issue
  //  5710 http://d.puremagic.com/issues/show_bug.cgi?id=5710.  Live with this and use the string syntax
  //  for specifying a lambda function.
  //
  //immutable pi = 4.0 * delta * taskPool.reduce ! ( ( a , b ) { return a + b ; } ) ( map ! getTerm ( iota ( n ) ) ) ;
  immutable pi = 4.0 * delta * taskPool.reduce ! ( "a + b" ) ( map ! getTerm ( iota ( n ) ) ) ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Parallel Reduce pi = %.18f" , pi ) ;
  writefln ( "==== D Parallel Reduce iteration count = %d" , n ) ;
  writefln ( "==== D Parallel Reduce elapse = %f" , elapseTime ) ;
  return 0 ;
}
