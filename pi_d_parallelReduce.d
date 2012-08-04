/*
 *  A D program to calculate π using quadrature as a parallel reduce of individual expression evaluations
 *  with no manual batching.
 *
 *  Copyright © 2011–2012 Russel Winder
 */

//  This version originally due to David Simcha, stemming from various emails on the various D email lists
//  and reified in the documentation for std.parallelism: http://dlang.org/phobos/std_parallelism.html,
//  http://cis.jhu.edu/~dsimcha/d/phobos/std_parallelism.html

import std.algorithm ;
import std.datetime ;
import std.parallelism ;
import std.range ;

import output_d ;

int main ( immutable string[] args ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  //  There is a problem using a lambda function here.  David Simcha reports it is a consequence of issue
  //  5710 http://d.puremagic.com/issues/show_bug.cgi?id=5710.  Live with this and use the string syntax
  //  for specifying a lambda function.
  //immutable pi = 4.0 * delta * taskPool.reduce ! ( ( a , b ) { return a + b ; } ) (
  //immutable pi = 4.0 * delta * taskPool.reduce ! ( ( a , b ) => a + b ) (
  immutable pi = 4.0 * delta * taskPool.reduce ! ( "a + b" ) (
      map ! ( ( int i ) { immutable x = ( i - 0.5 ) * delta ; return 1.0 / ( 1.0 + x * x ) ; } ) ( iota ( n ) ) ) ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  output ( __FILE__ , pi , n , elapseTime ) ;
  return 0 ;
}
