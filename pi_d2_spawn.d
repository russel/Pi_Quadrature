/*
 *  A D program to calculate Pi using quadrature as a spawn-based algorithm.  Make use of Actor Model
 *  message passing capability.
 *
 *  Copyright © 2010 Russel Winder
 */

import std.concurrency ;
import std.date ;
import std.stdio ;

//  There seems to be a problem with 2.048 and earlier in that having a real parameter to this function which
//  gets spawned really screws things up.

void partialSum ( Tid parent , const long start , const long end , const double d ) {  //  const real delta ) {
  real delta = d ;  //  Hack, hack.  Using real instead of double makes the tight loop a lot faster.
  auto sum = 0.0 ;  //  Using a double here rather than a real makes things a lot faster!!!!!
  foreach ( i ; start .. end ) {
    invariant x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  parent.send  ( sum ) ;
}

void execute ( const int numberOfThreads ) {
  invariant n = 1000000000L ;  //  Using int here instead of long would make this 40% faster but C and C++ use long.
  invariant delta = 1.0 / n ;
  invariant startTime = getUTCtime ( ) ;
  invariant sliceSize = n / numberOfThreads ;
  auto threads = new Tid[numberOfThreads] ;  
  foreach ( i ; 0 .. numberOfThreads ) { threads[i] = spawn ( & partialSum , thisTid , 1 + i * sliceSize , ( i + 1 ) * sliceSize , cast ( double ) ( delta ) ) ; }
  auto sum = 0.0L ;
  foreach ( thread ; threads ) { sum += receiveOnly!double ( ) ; }
  invariant pi = 4.0 * sum * delta ;
  invariant elapseTime = ( cast (real) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Spawn pi = %.18f" , pi ) ;
  writefln ( "==== D Spawn iteration count = %d" , n ) ;
  writefln ( "==== D Spawn elapse = %f" , elapseTime ) ;
  writefln ( "==== D Spawn thread count = %d" , numberOfThreads ) ;
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
