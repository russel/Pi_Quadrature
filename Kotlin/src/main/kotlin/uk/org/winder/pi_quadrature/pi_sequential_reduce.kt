/*
 *  Calculation of π using quadrature realized with reduce.
 *
 *  Copyright © 2013–2016  Russel Winder
 */

package uk.org.winder.pi_quadrature

/*

This does not compile just now so comment out.

fun main(args:Array<String>) {
  val n = 1000000000
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime()
  // cf. KT-8067 Type inference and overload resolution with reduce function,
  // https://youtrack.jetbrains.com/issue/KT-8067
  // Also cf. KT-11168 Problem resolving types with reduce
  // https://youtrack.jetbrains.com/issue/KT-11168
  val r = (1.0..n.toDouble()).reduce{t:Double, i:Double ->
    val x = (i - 0.5) * delta
    t + 1.0 / (1.0 + x * x)
  }
  val pi = 4.0 * delta * r
  val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  out("Sequential Reduce", pi, n, elapseTime)
}
*/