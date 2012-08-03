/*
 *  A D program to calculate Pi using quadrature as a spawn-based algorithm.  Make use of Actor Model
 *  message passing capability.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import std.concurrency ;
import std.datetime ;

import output_d ;

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
  foreach ( i ; 0 .. numberOfTasks ) {
    spawn ( & partialSum , thisTid , cast ( immutable ( int ) ) i , sliceSize , delta ) ;
  }
  auto sum = 0.0 ;
  foreach ( i ; 0 .. numberOfTasks ) { sum += receiveOnly ! double ( ) ; }
  immutable pi = 4.0 * delta * sum ;
  stopWatch.stop ( ) ;
  immutable elapseTime = stopWatch.peek ( ).hnsecs * 100e-9 ;
  output ( __FILE__ , pi , n , elapseTime , numberOfTasks ) ;
}

int main ( immutable string[] args ) {
  execute ( 1 ) ;
  execute ( 2 ) ;
  execute ( 8 ) ;
  execute ( 32 ) ;
  return 0 ;
}
