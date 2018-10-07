/*
 *  Calculation of π using quadrature realized with coroutines.
 *
 *  Copyright © 2018  Russel Winder
 */

package uk.org.winder.pi_quadrature

import kotlinx.coroutines.experimental.async
import kotlinx.coroutines.experimental.runBlocking

fun main(args:Array<String>) {
	val n = 10_000_000  //  Can't use the full 1_000_000_000 as that OOMEs
	val delta = 1.0 / n
	val startTimeNanos = System.nanoTime()
	val values = (1..n).map { i ->
		async {
			val x = (i - 0.5) * delta
			1.0 / (1.0 + x * x)
		}
	}
	runBlocking {
		val pi = 4.0 * delta * values.sumByDouble{it.await()}
		val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
		out("Coroutines", pi, n, elapseTime)
	}
}
