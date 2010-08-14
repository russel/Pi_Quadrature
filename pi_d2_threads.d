/*
 *  A D program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import std.bind ;
import std.date ;
import std.stdio ;

import core.thread ;

shared real sum ;
shared Object sumMutex ;

void partialSum ( const long start , const long end , const real delta ) {
  auto localSum = 0.0L ; // Using a real here rather than a double makes things twice as fast!!!!!
  foreach ( i ; start .. end ) {
    invariant x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  synchronized ( sumMutex ) { sum += localSum ; }
}

void execute ( const int numberOfThreads ) {
  invariant n = 1000000000L ; // Using int here instead of long would make this 40% faster but C and C++ use long.
  invariant delta = 1.0 / n ; //  Using 1.0L makes things twice as slow if sum is real.
  invariant startTime = getUTCtime ( ) ;
  invariant sliceSize = n / numberOfThreads ;
  auto threads = new Thread[numberOfThreads] ;  
  foreach ( i ; 0 .. numberOfThreads ) { threads[i] = new Thread ( bind ( & partialSum , 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ) ; }
  foreach ( thread ; threads ) { thread.start ( ) ; }
  foreach ( thread ; threads ) { thread.join ( ) ; }
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
