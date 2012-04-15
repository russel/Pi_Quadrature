/*
 *  A D program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

import std.algorithm ;
import std.datetime ;
import std.range ;
import std.stdio ;

import core.thread ;

shared double sum ;
shared Object sumMutex ;

void partialSum ( immutable int id , immutable int sliceSize , immutable double delta ) {
  immutable start = 1 + id * sliceSize ;
  immutable end = ( id + 1 ) * sliceSize ;
  auto localSum = 0.0 ;
  foreach ( i ; start .. end + 1 ) {
    immutable x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  synchronized ( sumMutex ) { sum += localSum ; }
}

void execute ( immutable int numberOfThreads ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  immutable sliceSize = n / numberOfThreads ;
  sum = 0.0 ;
  auto threadGroup = new ThreadGroup ( ) ;
  foreach ( i ; 0 .. numberOfThreads ) {
    void delegate ( ) closedPartialSum ( ) {
      immutable ii = i ;
      return delegate ( ) { partialSum ( ii , sliceSize , delta ) ; } ;
    }
    threadGroup.create ( closedPartialSum ) ;
  }
  threadGroup.joinAll( );
  immutable pi = 4.0 * delta * sum ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Threads Global State ThreadGroup pi = %.18f" , pi ) ;
  writefln ( "==== D Threads Global State ThreadGroup iteration count = %d" , n ) ;
  writefln ( "==== D Threads Global State ThreadGroup elapse = %f" , elapseTime ) ;
  writefln ( "==== D Threads Global State ThreadGroup thread count = %d" , numberOfThreads ) ;
}

int main ( immutable string[] args ) {
  sumMutex = new shared ( Object ) ;
  execute ( 1 ) ;
  writeln ( ) ;
  execute ( 2 ) ;
  writeln ( ) ;
  execute ( 8 ) ;
  writeln ( ) ;
  execute ( 32 ) ;
  return 0 ;
}
