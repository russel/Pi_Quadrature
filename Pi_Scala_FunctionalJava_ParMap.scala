/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009--2010 Russel Winder
 */

//  This seems to execute fine but it takes a very long time to terminate after the results have been
//  output.  Very strange and :-((

import fj.control.parallel.Strategy
import fj.control.parallel.ParModule
import java.util.concurrent.Executors

import fj.F
import fj.F2
import fj.data.List

object Pi_Scala_FunctionalJava_ParMap {
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = new F[java.lang.Integer,Double] {
      def f ( indexOfWrongType : java.lang.Integer ) : Double = {
        val index : Int = indexOfWrongType.intValue
        val start = 1 + index * sliceSize
        val end = ( index + 1 ) * sliceSize 
        var sum = 0.0
        for ( i <- start to end ) {
          val x = ( i - 0.5 ) * delta
          sum += 1.0 / ( 1.0 + x * x )
        }
        sum
      }
    }
    val sum = new F2[Double,Double,Double] {
      def f ( a : Double , b : Double ) : Double = { a + b }
    }
    val pi = 4.0 * delta * ParModule
      .parModule ( Strategy.executorStrategy[fj.Unit] ( Executors.newCachedThreadPool ) )
      .parMap ( List.range ( 0 , numberOfThreads ) , partialSum )
      .claim
      .foldLeft1 ( sum )
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala FunctionalJava ParMap pi = " + pi )
    println ( "==== Scala FunctionalJava ParMap iteration count = " + n )
    println ( "==== Scala FunctionalJava ParMap elapse = " + elapseTime )
    println ( "==== Scala FunctionalJava ParMap processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala FunctionalJava ParMap threads count = " + numberOfThreads )
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
