/*
 *  A D program to calculate Pi using quadrature as a spawn-based algorithm.  Make use of Actor Model
 *  message passing capability.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

//import std.algorithm ;
import std.concurrency ;
import std.datetime ;
//import std.range ;
import std.stdio ;

void partialSum ( Tid parent , immutable int id , immutable int sliceSize , immutable double delta ) {
  immutable start = 1 + id * sliceSize ;
  immutable end = ( id + 1 ) * sliceSize ;
  auto sum = 0.0 ;
  foreach ( i ; start .. end + 1 ) {
    immutable x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  parent.send  ( sum ) ;
}

void execute ( immutable int numberOfTasks ) {
  immutable n = 1000000000 ;
  immutable delta = 1.0 / n ;
  StopWatch stopWatch ;
  stopWatch.start ( ) ;
  immutable sliceSize = n / numberOfTasks ;
  //
  //  The following leads to serialization of computation, so do things with a foreach :-((  
  //
  //auto tasks = map ! ( ( i ) { return spawn ( & partialSum , thisTid , i , sliceSize , delta ) ; } ) ( iota ( numberOfTasks ) ) ;
  auto tasks = new Tid[numberOfTasks] ;  
  foreach ( i ; 0 .. numberOfTasks ) {
    immutable ii = i ;
    tasks[i] = spawn ( & partialSum , thisTid , ii , sliceSize , delta ) ;
  }
  auto sum = 0.0 ;
  foreach ( task ; tasks ) { sum += receiveOnly ! double ( ) ; }
  immutable pi = 4.0 * delta * sum ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
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
