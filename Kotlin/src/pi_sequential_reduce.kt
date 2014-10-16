/*
 *  Calculation of π using quadrature realized with map.
 *
 *  Copyright © 2013, 2014  Russel Winder
 */

package uk.org.winder.pi_quadrature.pi_sequential_reduce

import uk.org.winder.pi_quadrature.out

fun main(args:Array<String>) {
  val n = 1000000000
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime()
  val pi = 4.0 * delta * (1.0..n).reduce({t, i ->
    val x = (i - 0.5) * delta
    t + 1.0 / (1.0 + x * x)
  })
  val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  out("pi_sequential_reduce", pi, n, elapseTime)
}
