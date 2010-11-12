/*
 *  A D program to calculate Pi using quadrature as a spawn-based algorithm.  Make use of Actor Model
 *  message passing capability.
 *
 *  Copyright © 2010 Russel Winder
 */

import std.concurrency ;
import std.date ;
import std.stdio ;

void partialSum ( Tid parent , immutable long start , immutable long end , immutable real delta ) {
  auto sum = 0.0L ;
  foreach ( i ; start .. end ) {
    immutable x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  parent.send  ( sum ) ;
}

void execute ( immutable int numberOfThreads ) {
  immutable n = 1000000000L ;
  immutable delta = 1.0 / n ;
  immutable startTime = getUTCtime ( ) ;
  immutable sliceSize = n / numberOfThreads ;
  auto threads = new Tid[numberOfThreads] ;  
  foreach ( i ; 0 .. numberOfThreads ) { threads[i] = spawn ( & partialSum , thisTid , 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ; }
  auto sum = 0.0L ;
  foreach ( thread ; threads ) { sum += receiveOnly!real ( ) ; }
  immutable pi = 4.0 * sum * delta ;
  immutable elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Spawn pi = %.18f" , pi ) ;
  writefln ( "==== D Spawn iteration count = %d" , n ) ;
  writefln ( "==== D Spawn elapse = %f" , elapseTime ) ;
  writefln ( "==== D Spawn thread count = %d" , numberOfThreads ) ;
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
