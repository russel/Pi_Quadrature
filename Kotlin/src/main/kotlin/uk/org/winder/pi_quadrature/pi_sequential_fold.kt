/*
 *  Calculation of π using quadrature realized with reduce.
 *
 *  Copyright © 2013–2016  Russel Winder
 */

package uk.org.winder.pi_quadrature

fun main(args:Array<String>) {
	val n = 1000000000
	val delta = 1.0 / n
	val startTimeNanos = System.nanoTime()
	val pi = 4.0 * delta * (1..n).fold(1.0){t, i ->
		val x = (i - 0.5) * delta
		t + 1.0 / (1.0 + x * x)
	}
	val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
	out("Sequential Fold", pi, n, elapseTime)
}
