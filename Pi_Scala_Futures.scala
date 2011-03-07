/*
 *  Calculation of Pi using quadrature realized with futures.
 *
 *  Copyright © 2009--2010 Russel Winder
 */

import scala.actors.Future
import scala.actors.Futures

object Pi_Scala_Futures {
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSums = for ( index <- 0 until numberOfThreads ) yield
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
    val pi = 4.0 * delta * ( ( 0.0 /: partialSums ) ( _ + _ ( ) ) )
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Futures pi = " + pi )
    println ( "==== Scala Futures iteration count = " + n )
    println ( "==== Scala Futures elapse = " + elapseTime )
    println ( "==== Scala Futures processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Futures threads count = " + numberOfThreads )
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
