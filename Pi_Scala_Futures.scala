/*
 *  Calculation of Pi using quadrature realized with futures.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import scala.actors.Future
import scala.actors.Futures

object Pi_Scala_Futures extends Application {
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSums = for ( index <- 0 until numberOfThreads ) yield
      Futures.future {
        var i = 1 + index * sliceSize
        val end = ( index + 1 ) * sliceSize 
        var sum = 0.0
        while ( i <=  end ) {
          val x = ( i - 0.5 ) * delta
          sum += 1.0 / ( 1.0 + x * x )
          i += 1
        }
        sum
      }
    val pi = 4.0 * ( ( 0.0 /: partialSums ) ( _ + _ ( ) ) ) * delta
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Futures pi = " + pi )
    println ( "==== Scala Futures iteration count = " + n )
    println ( "==== Scala Futures elapse = " + elapseTime )
    println ( "==== Scala Futures processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Futures threads count = " + numberOfThreads )
  }
  execute ( 1 )
  println
  execute ( 2 )
  println
  execute ( 8 )
  println
  execute ( 32 )
}
