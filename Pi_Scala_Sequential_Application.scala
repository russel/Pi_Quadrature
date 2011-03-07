/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm using a while.
 *
 *  Copyright © 2009--2010 Russel Winder
 */

//  When using the idiom of extending Application to create a main procedure, variables are represented as
//  methods and this means performance seriously plummets.

object Pi_Scala_Sequential_Application extends Application {
  val n = 1000000000
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime
  var sum = 0.0
  for ( i <- 1 to n ) {
    val x = ( i - 0.5 ) * delta
    sum += 1.0 / ( 1.0 + x * x )
  }
  val pi = 4.0 * delta * sum
  def elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
  println ( "==== Scala Sequential Application pi = " + pi )
  println ( "==== Scala Sequential Application iteration count = " + n )
  println ( "==== Scala Sequential Applciation elapse = " + elapseTime )
}
