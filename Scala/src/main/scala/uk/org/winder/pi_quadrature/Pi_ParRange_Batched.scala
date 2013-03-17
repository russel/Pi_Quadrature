/*
 *  Calculation of π using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import Output.out

import scala.collection.parallel.immutable.ParRange

object Pi_ParArray_Batched {

  def execute(numberOfThreads:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = (index:Int) => {
      val start = 1 + index * sliceSize
      val end = (index + 1) * sliceSize
      var sum = 0.0
      for (i <- start to end) {
        val x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
      }
      sum
    }
    val pi = 4.0 * delta * (1 to numberOfThreads par).map(partialSum).sum
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_ParallelMap_Batched", pi, n, elapseTime, numberOfThreads)
  }

  def main(args:Array[String]) {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }

}
