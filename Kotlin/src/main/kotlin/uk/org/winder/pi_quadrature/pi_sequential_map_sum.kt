/*
 *  Calculation of π using quadrature realized with map and reduce.
 *
 *  Copyright © 2013–2016, 2018  Russel Winder
 */

package uk.org.winder.pi_quadrature

fun main(args:Array<String>) {
	val n = 1_000_000_000
	val delta = 1.0 / n
	val startTimeNanos = System.nanoTime()
	// Need to *not* map all the items to a data structure!
	// Get better performance using reduce rather than sum.
	// cf. KT-11169 reduce faster than sum?
	// https://youtrack.jetbrains.com/issue/KT-11169
	val pi = 4.0 * delta * (1..n).asSequence().map{i ->
		val x = (i - 0.5) * delta
		1.0 / (1.0 + x * x)
	}.sum()
	val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
	out("Sequential Map Sum", pi, n, elapseTime)
}
