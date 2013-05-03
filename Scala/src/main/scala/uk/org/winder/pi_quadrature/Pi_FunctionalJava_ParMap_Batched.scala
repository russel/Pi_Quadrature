/*
 *  Calculation of π using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

//  TODO: This seems to execute fine but it takes a very long time to terminate after the results have been
//  output.  Very strange and :-((

import fj.control.parallel.Strategy
import fj.control.parallel.ParModule
import java.util.concurrent.Executors

import fj.F
import fj.F2
import fj.data.List

import Output.out

object Pi_FunctionalJava_ParMap_Batched {

  def execute(numberOfThreads:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = new F[java.lang.Integer,Double] {
      def f(indexOfWrongType:java.lang.Integer):Double = {
        val index:Int = indexOfWrongType.intValue
        val start = 1 + index * sliceSize
        val end = (index + 1) * sliceSize
        var sum = 0.0
        for (i <- start to end) {
          val x = (i - 0.5) * delta
          sum += 1.0 / (1.0 + x * x)
        }
        sum
      }
    }
    val sum = new F2[Double,Double,Double] {
      def f(a:Double, b:Double):Double = { a + b }
    }
    val pi = 4.0 * delta * ParModule
      .parModule(Strategy.executorStrategy[fj.Unit](Executors.newCachedThreadPool))
      .parMap(List.range(0, numberOfThreads), partialSum)
      .claim
      .foldLeft1(sum)
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_FunctionalJava_ParMap_Batched", pi, n, elapseTime, numberOfThreads)
  }

  def main(args:Array[String]) {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }

}
