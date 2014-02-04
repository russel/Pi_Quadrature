/*
 *  Calculation of π using quadrature realized with a scatter/gather approach using futures.
 *
 *  Copyright © 2009–2014  Russel Winder
 */

package uk.org.winder.pi_quadrature

import scala.concurrent.{Await, Future, future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import Output.out

object Pi_Futures {

  def execute(numberOfWorkers:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkers
    val partialSums = for (index <- 0 until numberOfWorkers) yield
      future {
        val start = 1 + index * sliceSize
        val end = (index + 1)* sliceSize
        var sum = 0.0
        for (i <- start to end){
          val x = (i - 0.5)* delta
          sum += 1.0 / (1.0 + x * x)
        }
        sum
      }
    val pi = 4.0 * delta * Await.result(Future.reduce(partialSums)(_ + _), Duration.Inf)
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_Futures", pi, n, elapseTime, numberOfWorkers)
  }

  def main(args:Array[String]){
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }

}
