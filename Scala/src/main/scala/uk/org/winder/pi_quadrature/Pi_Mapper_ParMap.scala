/*
 *  Calculation of π using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import scala.actors.Futures
import scala.reflect.ClassTag

import Output.out

object Pi_Mapper_ParMap {

  class Mapper[A,B:ClassTag](data:List[A] , function:A => B) {
    val pmap = {
      val buffer = new Array[B](data.length)
      val mappers = for (index <- 0 until data.length) yield Futures.future { buffer(index) = function(data(index)) }
      for (mapper <- mappers) mapper()
      buffer
    }
  }

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
    val pi = 4.0 * delta * (((new Mapper[Int,Double](List.range(0 , numberOfThreads) , partialSum)).pmap) reduceLeft(_ + _))
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("Pi_Mapper_ParMap" , pi , n , elapseTime , numberOfThreads)
  }

  def main(args:Array[String]) {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }

}
