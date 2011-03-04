/*
 *  A D program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

import std.datetime ;
import std.stdio ;

import core.thread ;

shared double sum ;
shared Object sumMutex ;

void partialSum ( immutable long start , immutable long end , immutable double delta ) {
  auto localSum = 0.0 ;
  foreach ( i ; start .. end ) {
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
  auto threads = new Thread[numberOfThreads] ;  
  foreach ( i ; 0 .. numberOfThreads ) {
    //
    //  In order to capture the value of i it is necessary to create a function that returns a delegate to
    //  be used as the function to pass in to the thread object.  This is analogous to what has to be done
    //  in Python, but it wopuld be much better if it were simpler as in Groovy or Ruby.
    //
    void delegate ( ) closedPartialSum ( ) {
      immutable id = i ;
      return ( ) { partialSum ( 1 + id * sliceSize , ( id + 1 ) * sliceSize , delta ) ; } ;
    }
    threads[i] = new Thread ( closedPartialSum ) ;
  }
  foreach ( thread ; threads ) { thread.start ( ) ; }
  foreach ( thread ; threads ) { thread.join ( ) ; }
  immutable pi = 4.0 * sum * delta ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  writefln ( "==== D Threads pi = %.18f" , pi ) ;
  writefln ( "==== D Threads iteration count = %d" , n ) ;
  writefln ( "==== D Threads elapse = %f" , elapseTime ) ;
  writefln ( "==== D Threads thread count = %d" , numberOfThreads ) ;
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