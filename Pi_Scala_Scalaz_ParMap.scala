/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2010 Russel Winder
 */

//  This seems to execute fine but it takes a very long time to terminate after the results have been
//  output.  Very strange and :-((

import scalaz._
import Scalaz._

//  This pulls in all the implicit values needed by the parmap function to make use of an Executor.
import scalaz.concurrent.Strategy.Executor

//  But now we will have to specify which Executor.
import java.util.concurrent.Executors

object Pi_Scala_Scalaz_ParMap {
  implicit val pool = Executors.newCachedThreadPool
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = ( index : Int ) => {
      val start = 1 + index * sliceSize
      val end = ( index + 1 ) * sliceSize 
      var sum = 0.0
      for ( i <- start to end ) {
        val x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
      }
      sum
    }
    val pi = 4.0 * delta * List.range ( 0 , numberOfThreads ).parMap ( partialSum ).get.reduceLeft ( _ + _ )
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Scalaz ParMap pi = " + pi )
    println ( "==== Scala Scalaz ParMap iteration count = " + n )
    println ( "==== Scala Scalaz ParMap elapse = " + elapseTime )
    println ( "==== Scala Scalaz ParMap processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Scalaz ParMap threads count = " + numberOfThreads )
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
