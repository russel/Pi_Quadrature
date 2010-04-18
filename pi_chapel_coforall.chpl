/*
 *  A Chapel program to calculate Pi using quadrature as a coforall-based algorithm.
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
  // Partition the evaluation into an explicit coforall followed by a reduce so as to get multi-threaded
  // behaviour.  When last checked (2010-04-10 Chapel v1.0), reduce is always single threaded.
  coforall i in eachProcessor do results[i] = partialSum ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ;
  const pi = 4.0 * ( + reduce results ) * delta ;
  timer.stop ( ) ;
  writeln ( "==== Chapel Coforall pi = " , pi ) ;
  writeln ( "==== Chapel Coforall iteration count = " , n ) ;
  writeln ( "==== Chapel Coforall elapse = " , timer.elapsed ( ) ) ;
  writeln ( "==== Chapel Coforall task count = " , numberOfTasks ) ;
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
