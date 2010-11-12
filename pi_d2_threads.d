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

void partialSum ( immutable long start , immutable long end , immutable real delta ) {
  auto localSum = 0.0L ;
  foreach ( i ; start .. end ) {
    immutable x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  synchronized ( sumMutex ) { sum += localSum ; }
}

void execute ( immutable int numberOfThreads ) {
  immutable n = 1000000000L ;
  immutable delta = 1.0 / n ;
  immutable startTime = getUTCtime ( ) ;
  immutable sliceSize = n / numberOfThreads ;
  auto threads = new Thread[numberOfThreads] ;  
  foreach ( i ; 0 .. numberOfThreads ) { threads[i] = new Thread ( bind ( & partialSum , 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ) ; }
  foreach ( thread ; threads ) { thread.start ( ) ; }
  foreach ( thread ; threads ) { thread.join ( ) ; }
  immutable pi = 4.0 * sum * delta ;
  immutable elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Threads pi = %f" , pi ) ;
  writefln ( "==== D Threads iteration count = %d" , n ) ;
  writefln ( "==== D Threads elapse = %f" , elapseTime ) ;
  writefln ( "==== D Threads thread count = %d" , numberOfThreads ) ;
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
