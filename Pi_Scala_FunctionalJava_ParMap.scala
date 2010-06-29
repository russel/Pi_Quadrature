/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import fj.control.parallel.Strategy
import fj.control.parallel.ParModule
import java.util.concurrent.Executors

import fj.F
import fj.F2
import fj.data.List

object Pi_Scala_FunctionalJava_ParMap extends Application {
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = new F[java.lang.Integer,Double] {
      def f ( indexOfWrongType : java.lang.Integer ) : Double = {
        val index : Int = indexOfWrongType.intValue
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
    }
    val adder = new F2[Double,Double,Double] {
      def f ( a : Double , b : Double ) : Double = { a + b }
    }
    val sum = ParModule
        .parModule ( Strategy.executorStrategy[fj.Unit] ( Executors.newCachedThreadPool ) )
        .parMap ( List.range ( 0 , numberOfThreads ) , partialSum )
        .claim
        .foldLeft1 ( adder )
    val pi = 4.0 * sum * delta
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Functional Java Parallel Map pi = " + pi )
    println ( "==== Scala Functional Java Parallel Map iteration count = " + n )
    println ( "==== Scala Functional Java Parallel Map elapse = " + elapseTime )
    println ( "==== Scala Functional Java Parallel Map processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Functional Java Parallel Map threads count = " + numberOfThreads )
  }
  execute ( 1 )
  println
  execute ( 2 )
  println
  execute ( 8 )
  println
  execute ( 32 )
}
