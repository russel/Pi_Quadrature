/*
 *  Calculation of π using quadrature realized with a scatter/gather approach using threads.
 *
 *  Copyright © 2009–2014  Russel Winder
 */

package uk.org.winder.pi_quadrature

import java.util.concurrent.locks.ReentrantLock

import Output.out

object Pi_Threads {

	def execute(numberOfTasks:Int) {
		val n = 1000000000
		val delta = 1.0 / n
		val startTimeNanos = System.nanoTime
		val sliceSize = n / numberOfTasks
		var sum = 0.0
		val lock = new ReentrantLock
		var threads = for (index <- 0 until numberOfTasks) yield
			new Thread(new Runnable {
				def run() {
					val start = 1 + index * sliceSize
					val end = (index + 1) * sliceSize
					var localSum = 0.0
					for (i <- start to end) {
						val x = (i - 0.5) * delta
						localSum += 1.0 / (1.0 + x * x)
					}
					lock.lock
					sum += localSum
					lock.unlock
				}
			})
		threads.foreach(t => t.start)
		threads.foreach(t => t.join)
		val pi = 4.0 * delta * sum
		val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
		out("Pi_Threads" , pi , n, elapseTime , numberOfTasks)
	}

	def main(args:Array[String]) {
		execute(1)
		execute(2)
		execute(8)
		execute(32)
	}

}
