/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm using a for statement.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

object Pi_Scala_Sequential_Main_For {
  def main ( args : Array[String] ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    var sum = 0.0
    for ( i <- 1 to n ) {
      val x = ( i - 0.5 ) * delta
      sum += 1.0 / ( 1.0 + x * x )
    }
    val pi = 4.0 * delta * sum
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Sequential Main For pi = " + pi )
    println ( "==== Scala Sequential Main For iteration count = " + n )
    println ( "==== Scala Sequential Main For elapse = " + elapseTime )
  }
}
