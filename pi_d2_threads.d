/*
 *  A D program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import std.bind ;
import std.date ;
import std.stdio ;

import core.thread ;

real sum ;
Object sumMutex ;

void partialSum ( const long start , const long end , const real delta ) {
  auto localSum = 0.0 ;
  //  Have to have long here not auto to avoid the pi_d2_threads.d(18): Error: variable pi_d2_threads.partialSum.i cannot modify const
  for ( long i = start ; i <= end ; ++i ) {
    invariant x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  synchronized ( sumMutex ) { sum += localSum ; }
}

void execute ( const int numberOfThreads ) {
  invariant n = 1000000000L ;
  invariant delta = 1.0 / n ;
  invariant sliceSize = n / numberOfThreads ;
  auto threads = new Thread[numberOfThreads] ;  
  invariant startTime = getUTCtime ( ) ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { new Thread ( bind ( & partialSum , 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ) ; }
  foreach ( thread ; threads ) { thread.wait ( ) ; }
  invariant pi = 4.0 * sum * delta ;
  invariant elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Threads pi = %f" , pi ) ;
  writefln ( "==== D Threads iteration count = %d" , n ) ;
  writefln ( "==== D Threads elapse = %f" , elapseTime ) ;
  writefln ( "==== D Threads thread count = %d" , numberOfThreads ) ;
}

int main ( string[] args ) {
  execute ( 1 ) ;
  writeln ( ) ;
  execute ( 2 ) ;
  writeln ( ) ;
  execute ( 8 ) ;
  writeln ( ) ;
  execute ( 32 ) ;
  return 0 ;
}
