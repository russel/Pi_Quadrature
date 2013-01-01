/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using futures.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import scala.actors.Future
import scala.actors.Futures

import Output.out

object Pi_Futures {

  def execute(numberOfWorkers:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkers
    val partialSums = for (index <- 0 until numberOfWorkers) yield
      Futures.future {
        val start = 1 + index * sliceSize
        val end = (index + 1)* sliceSize
        var sum = 0.0
        for (i <- start to end){
          val x = (i - 0.5)* delta
          sum += 1.0 / (1.0 + x * x)
        }
        sum
      }
    //  NB The second parameter of the function passed into the reduce is a Future[Double] which is a
    //  function that must be called to obtain the value.  Hence _+_() instead of _+_.
    val pi = 4.0 * delta * ((0.0 /: partialSums)(_ + _()))
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
