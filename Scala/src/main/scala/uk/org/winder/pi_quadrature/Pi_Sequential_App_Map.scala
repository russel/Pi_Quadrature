/*
 *  Calculation of π using quadrature realized with map.
 *
 *  Copyright © 2009–2014  Russel Winder
 */

package uk.org.winder.pi_quadrature

import Output.out

object Pi_Sequential_App_Map extends App {

  val n = 100000000  // 10 times fewer for performance reasons.
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime
  def f(i:Int):Double = {
    val x = (i - 0.5) * delta
    1.0 / (1.0 + x * x)
  }
  val pi = 4.0 * delta * (0 until n).iterator.map(f).sum
  val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
  out("Pi_Sequential_App_Map", pi, n, elapseTime)

}
