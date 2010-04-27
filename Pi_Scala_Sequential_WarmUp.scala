/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm using a while.
 *  Show the JVM warm up by executing the same thing a number of times.
 *
 *  Copyright © 2009-10 Russel Winder
 */

object Pi_Scala_Sequential_WarmUp {
  def execute {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    var sum = 0.0
    var i = 1
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
  def main ( args : Array[String] ) {
    execute
    println
    execute
    println
    execute
    println
    execute
  }
}
