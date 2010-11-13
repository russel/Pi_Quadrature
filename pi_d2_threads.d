/*
 *  A D program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009-10 Russel Winder
 */

//  Partial application using std.bind.bind fails to work in D 2.050 and earlier.  So we make use of an
//  anonymous function as a "delegate."

//import std.bind ;
import std.date ;
import std.stdio ;

import core.thread ;

//  As at 2010-11-13 D 2.050 is a 32-bit system generating 32-bit code.  Using long rather than int makes
//  this quite a lot slower than the equivalents in C and C++.  64-bit D is due "very soon now".

shared double sum ;
shared Object sumMutex ;

void partialSum ( immutable int start , immutable int end , immutable double delta ) {
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
  immutable startTime = getUTCtime ( ) ;
  immutable sliceSize = n / numberOfThreads ;
  auto threads = new Thread[numberOfThreads] ;  
  //foreach ( i ; 0 .. numberOfThreads ) { threads[i] = new Thread ( bind ( & partialSum , 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ) ; }
  foreach ( i ; 0 .. numberOfThreads ) { threads[i] = new Thread ( ( ) { return partialSum ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ; } ) ; }
  foreach ( thread ; threads ) { thread.start ( ) ; }
  foreach ( thread ; threads ) { thread.join ( ) ; }
  immutable pi = 4.0 * sum * delta ;
  immutable elapseTime = ( cast ( double ) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Threads pi = %.18f" , pi ) ;
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
