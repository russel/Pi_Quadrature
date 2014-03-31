package uk.org.winder.pi_quadrature.pi_sequential_reduce

import uk.org.winder.pi_quadrature.out

fun main(args:Array<String>) {
  val n = 10000000  //  100 times fewer  due to speed issues.
  val delta = 1.0 / n
  val startTimeNanos = System.nanoTime()
  val pi = 4.0 * delta * (1.0..n).reduce({t, i ->
    val x = (i - 0.5) * delta
    t + 1.0 / (1.0 + x * x)
  })
  val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  out("pi_sequential_reduce", pi, n, elapseTime)
}
