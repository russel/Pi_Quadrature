/*
 *  Calculation of π using quadrature realized with a scatter/gather approach using an actor system.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import akka.actor.{Actor, ActorDSL, ActorSystem}

import Output.out

object Pi_Actors_DSL extends App {

  implicit val system = ActorSystem("PiActorsDSL")

  /*
  def execute(numberOfWorkerActors:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkerActors
    val accumulator = ActorDSL.actor {
      var sum = 0.0
      calculators.foreach(calculator => receive { case d => sum += d.asInstanceOf[Double] })
      val pi = 4.0 * delta * sum
      val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
      out("Pi_Actors", pi, n, elapseTime, numberOfWorkerActors)
      sequencer ! 0
    }
    for (index <- calculators.indices) {
      calculators(index) = ActorDSL.actor {
        val start = 1 + index * sliceSize
        val end = (index + 1) * sliceSize
        var sum = 0.0
        for (i <- start to end) {
          val x = (i - 0.5) * delta
          sum += 1.0 / (1.0 + x * x)
        }
        accumulator ! sum
      }
    }
  }

  val sequencer = ActorDSL.actor {
    execute(1)
    Actor.receive { case d => }
    execute(2)
    Actor.receive { case d => }
    execute(8)
    Actor.receive { case d => }
    execute(32)
  }
  */
}
