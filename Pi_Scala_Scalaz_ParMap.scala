/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import scalaz._
import Scalaz._

//  This pulls in all the implicit values needed by the parmap function to make use of an Executor.
import scalaz.concurrent.strategy.Executor.strategy

//  But now we will have to specify which Executor.
import java.util.concurrent.Executors

object Pi_Scala_Scalaz_ParMap extends Application {
  implicit val pool = Executors.newCachedThreadPool
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = ( index : Int ) => {
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
    val pi = 4.0 * List.range ( 0 , numberOfThreads ).parMap ( partialSum ).get.reduceLeft ( _ + _ ) * delta
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Scalaz Parallel Map pi = " + pi )
    println ( "==== Scala Scalaz Parallel Map iteration count = " + n )
    println ( "==== Scala Scalaz Parallel Map elapse = " + elapseTime )
    println ( "==== Scala Scalaz Parallel Map processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Scalaz Parallel Map threads count = " + numberOfThreads )
  }
  execute ( 1 )
  println
  execute ( 2 )
  println
  execute ( 8 )
  println
  execute ( 32 )
}
