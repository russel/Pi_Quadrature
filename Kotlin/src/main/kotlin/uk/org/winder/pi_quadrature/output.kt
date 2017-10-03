/*
 *  Output functions for calculations of π using quadrature.
 *
 *  Copyright © 2013–2015  Russel Winder
 */

package uk.org.winder.pi_quadrature

fun out(banner:String, pi:Double, n:Int, elapseTime:Double) {
	println("============================ " + banner)
	println("\tπ = " + pi)
	println("\titeration count = " + n)
	println("\telapse time = " + elapseTime)
}

fun out(banner:String, pi:Double, n:Int, elapseTime:Double, numberOfTasks:Int) {
	out(banner + ": task count" + numberOfTasks, pi, n, elapseTime)
	println("\tprocessor count = " + Runtime.getRuntime().availableProcessors())
}
