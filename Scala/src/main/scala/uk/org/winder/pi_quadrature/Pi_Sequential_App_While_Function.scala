/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm using a while statement.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import Output.out

object Pi_Sequential_App_While_Function extends App {

  def doStuff {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    var sum = 0.0
    var i = 1
    while (i <= n) {
      val x = (i - 0.5) * delta
      sum += 1.0 / (1.0 + x * x)
      i += 1
    }
    val pi = 4.0 * delta * sum
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_Sequential_App_While_Function", pi, n, elapseTime)
  }

  doStuff

}
