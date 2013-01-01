/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

//  This seems to execute fine but it takes a very long time to terminate after the results have been
//  output.  Very strange and :-((

import scalaz._
import Scalaz._

//  This pulls in all the implicit values needed by the parmap function to make use of an Executor.
import scalaz.concurrent.Strategy.Executor

//  But now we will have to specify which Executor.
import java.util.concurrent.Executors

import Output.out

object Pi_Scalaz_ParMap {

  implicit val pool = Executors.newCachedThreadPool

  def execute(numberOfThreads:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = (index:Int) => {
      val start = 1 + index * sliceSize
      val end = (index + 1) * sliceSize
      var sum = 0.0
      for (i <- start to end) {
        val x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
      }
      sum
    }
    val pi = 4.0 * delta * List.range(0, numberOfThreads).parMap(partialSum).get.reduceLeft(_ + _)
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_Scalaz_ParMap", pi, n, elapseTime, numberOfThreads)
  }

  def main(args:Array[String]) {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }

}
