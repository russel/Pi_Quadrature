/*
 *  A D program to calculate Pi using quadrature as a spawn-based algorithm.  Make use of Actor Model
 *  message passing capability.
 *
 *  Copyright © 2010-11 Russel Winder
 */

import std.concurrency ;
import std.date ;
import std.stdio ;

//  As at version 2.051 D is a 32-bit system generating 32-bit code.  Using long rather than int makes this
//  quite a lot slower than the equivalents in C and C++.  64-bit D is due "very soon now".

void partialSum ( Tid parent , immutable int start , immutable int end , immutable double delta ) {
  auto sum = 0.0 ;
  foreach ( i ; start .. end ) {
    immutable x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  parent.send  ( sum ) ;
}

void execute ( immutable int numberOfTasks ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  immutable startTime = getUTCtime ( ) ;
  immutable sliceSize = n / numberOfTasks ;
  auto tasks = new Tid[numberOfTasks] ;  
  foreach ( i ; 0 .. numberOfTasks ) { tasks[i] = spawn ( & partialSum , thisTid , 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ; }
  auto sum = 0.0 ;
  foreach ( task ; tasks ) { sum += receiveOnly ! double ( ) ; }
  immutable pi = 4.0 * sum * delta ;
  immutable elapseTime = ( cast ( double ) ( getUTCtime ( ) - startTime ) ) / ticksPerSecond ;
  writefln ( "==== D Spawn pi = %.18f" , pi ) ;
  writefln ( "==== D Spawn iteration count = %d" , n ) ;
  writefln ( "==== D Spawn elapse = %f" , elapseTime ) ;
  writefln ( "==== D Spawn task count = %d" , numberOfTasks ) ;
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
