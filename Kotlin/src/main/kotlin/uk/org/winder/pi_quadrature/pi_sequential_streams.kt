/*
 *  Calculation of π using quadrature realized with Java 8 parallel streams.
 *
 *  Copyright © 2013–2016  Russel Winder
 */

package uk.org.winder.pi_quadrature

import java.util.stream.IntStream

fun main(args:Array<String>) {
	val n = 1000000000
	val delta = 1.0 / n
	val startTimeNanos = System.nanoTime()
	val pi = 4.0 * delta * IntStream.range(1, n).mapToDouble{i ->
		val x = (i - 0.5) * delta
		1.0 / (1.0 + x * x)
	}.sum()
	val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
	out("Sequential Streams", pi, n, elapseTime)
}
