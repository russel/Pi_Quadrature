/*
 *  A Chapel program to calculate Pi using quadrature as a reduce-based algorithm.
 *
 *  Copyright © 2009 Russel Winder
 */

use Time ;

def execute ( param numberOfTasks : int ) {
  param n : int(64) = 1000000000 ;
  const delta : real = 1.0 / n ;
  param sliceSize : int(64) = n / numberOfTasks ;
  const eachProcessor : domain(1) = [ 0 .. ( numberOfTasks - 1 ) ] ;
  const results : [eachProcessor] real ;
  def partialSum ( const start : int(64) , const end : int(64), const delta : real ) : real {
    var sum : real = 0.0 ;
    for i in start .. end {
      sum += 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) ;
    }
    return sum ;
  }
  var timer : Timer ;
  timer.start ( ) ;
  //  As at 2009-04-17 with version 0.9 of Chapel, this is always handled in a single thread.
  //  See pi_chapel_coforall for a minor variant that actually uses multiple threads.
  const pi = 4.0 * ( + reduce [ i in eachProcessor ] partialSum ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ) * delta ;
  timer.stop ( ) ;
  writeln ( "==== Chapel Reduce pi = " , pi ) ;
  writeln ( "==== Chapel Reduce iteration count = " , n ) ;
  writeln ( "==== Chapel Reduce elapse = " , timer.elapsed ( ) ) ;
  writeln ( "==== Chapel Reduce task count = " , numberOfTasks ) ;
}

def main ( ) {
  execute ( 1 ) ;
  writeln ( ) ;
  execute ( 2 ) ;
  writeln ( ) ;
  execute ( 8 ) ;
  writeln ( ) ;
  execute ( 32 ) ;
}
