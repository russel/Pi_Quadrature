/*
 *  Calculation of Pi using quadrature realized with futures.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

//  TODO: Investigate why this takes longer with eight workers than two workers on a dual core machine.
//  There is of course a hint in that Futures are implemented with Actors and Actors shows the same problem.
//  cf. Pi_Scala_Actors.scala.

import scala.actors.Future
import scala.actors.Futures

object Pi_Scala_Futures {
  def execute ( numberOfWorkers : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkers
    val partialSums = for ( index <- 0 until numberOfWorkers ) yield
      Futures.future {
        val start = 1 + index * sliceSize
        val end = ( index + 1 ) * sliceSize 
        var sum = 0.0
        for ( i <- start to end ) {
          val x = ( i - 0.5 ) * delta
          sum += 1.0 / ( 1.0 + x * x )
        }
        sum
      }
    //
    //  NB The second parameter of the function passed into the reduce is a Future[Double] which is a
    //  function that must be called to obtain the value.  Hence _+_() instead of _+_.
    //
    val pi = 4.0 * delta * ( ( 0.0 /: partialSums ) ( _ + _ ( ) ) )
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Futures pi = " + pi )
    println ( "==== Scala Futures iteration count = " + n )
    println ( "==== Scala Futures elapse = " + elapseTime )
    println ( "==== Scala Futures processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Futures worker count = " + numberOfWorkers )
  }
  def main ( args : Array[String] ) {
    execute ( 1 )
    println
    execute ( 2 )
    println
    execute ( 8 )
    println
    execute ( 32 )
  }
}
