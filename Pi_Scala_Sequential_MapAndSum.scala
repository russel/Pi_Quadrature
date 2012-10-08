/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using futures.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

import SOutput.out

//  This works but is horrendously slow.

object Pi_Scala_Sequential_MapAndSum extends App {
  val n = 1000000000
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime
  def f(i:Int):Double = {
    val x = (i - 0.5) * delta
    1.0 / (1.0 + x * x)
  }
  val pi = 4.0 * delta * (0 until n).iterator.map(f).sum
  val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
  out("Pi_Scala_Futures", pi, n, elapseTime)
}
