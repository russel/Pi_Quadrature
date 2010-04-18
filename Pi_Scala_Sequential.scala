/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm using a while.
 *
 *  Copyright © 2009-10 Russel Winder
 */
//
//  Because of the way variables are represented as methods when using the idiom of extending Application to
//  create a main procedure, performance plummets.  We cannot afford this performance degredation so
//  use the "write a main method" idiom.
//
object Pi_Scala_Sequential {
  def main ( args : Array[String] ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    var sum = 0.0
    var i = 1l // Interesting behaviour if the l is missing.
    while ( i <= n ) {
      val x = ( i - 0.5 ) * delta
      sum += 1.0 / ( 1.0 + x * x )
      i += 1
    }
    val pi = 4.0 * sum * delta
    def elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Sequential pi = " + pi )
    println ( "==== Scala Sequential iteration count = " + n )
    println ( "==== Scala Sequential elapse = " + elapseTime )
  }
}
