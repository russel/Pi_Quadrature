/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm using a for.
 *  This is significantly slower than using a while.
 *
 *  Copyright © 2009-10 Russel Winder
 */
//
//  This version shows the effect of variables being represented as methods when using the idiom of
//  extending Application to create a main procedure, performance plummets.  We cannot afford this
//  performance degredation so use the "write a main method" idiom for the main code.  Keep this as evidence
//  though.
//
object Pi_Scala_Sequential_Alt {
  def main ( args : Array[String] ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    var sum = 0.0
    for ( i <- 1l to n ) {
      val x = ( i - 0.5 ) * delta
      sum += 1.0 / ( 1.0 + x * x )
    }
    val pi = 4.0 * sum * delta
    def elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Sequential Alt pi = " + pi )
    println ( "==== Scala Sequential Alt iteration count = " + n )
    println ( "==== Scala Sequential Alt elapse = " + elapseTime )
  }
}
