/*
 *  Calculation of π using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import Output.out

object Pi_ParMap {

  def execute(numberOfThreads:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val value = (index:Int) => {
      val x = (index - 0.5) * delta
      1.0 / (1.0 + x * x)
    }
    val pi = 4.0 * delta * (0 until n).par.map(value).sum
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_ParallelMap", pi, n, elapseTime, numberOfThreads)
  }

  def main(args:Array[String]) {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }

}
