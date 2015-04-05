/*
 *  Calculation of π using quadrature realized with map and reduce.
 *
 *  Copyright © 2013–2015  Russel Winder
 */

package uk.org.winder.pi_quadrature.pi_sequential_map_reduce

import uk.org.winder.pi_quadrature.out

fun main(args:Array<String>) {
  val n = 1000000000
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime()
  val pi = 4.0 * delta * (1..n).sequence().map{i ->
    val x = (i - 0.5) * delta
    1.0 / (1.0 + x * x)
  }.reduce{t, i -> t + i}
  val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  out("pi_sequential_map_reduce", pi, n, elapseTime)
}
