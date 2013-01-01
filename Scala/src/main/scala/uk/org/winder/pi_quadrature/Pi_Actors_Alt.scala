/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using an actor system.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

import scala.actors.Actor

import SOutput.out

class Accumulator(numberOfWorkerActors:Int, delta:Double, n:Int, startTimeNanos:Long, sequencer:Actor) extends Actor {
  def act {
    var sum = 0.0
    for (i <- 0 until numberOfWorkerActors) { Actor.receive { case d => sum += d.asInstanceOf[Double] } }
    //( 0 until numberOfWorkerActors).foreach(( i:Int) => Actor.receive { case d => sum += d.asInstanceOf[Double] })
    val pi = 4.0 * delta * sum
    val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
    out("PI_Scala_Actors_Alt", pi, n, elapseTime, numberOfWorkerActors)
    sequencer ! 0
  }
}

class Calculator(id:Int, sliceSize:Int, delta:Double, accumulator:Actor) extends Actor {
  def act {
    val start = 1 + id * sliceSize
    val end = (id + 1) * sliceSize
    var sum = 0.0
    for (i <- start to end) {
      val x = (i - 0.5) * delta
      sum += 1.0 / (1.0 + x * x)
    }
    accumulator ! sum
  }
}

object Pi_Scala_Actors_Alt extends App {
  def execute(numberOfWorkerActors:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkerActors
    val accumulator = new Accumulator(numberOfWorkerActors, delta, n, startTimeNanos, sequencer)
    accumulator.start
    val calculators = for (i <- 0 until numberOfWorkerActors) yield new Calculator(i, sliceSize, delta, accumulator)
    calculators.foreach { _.start }
  }
  val sequencer = Actor.actor {
    execute(1)
    Actor.receive { case d => }
    execute(2)
    Actor.receive { case d => }
    execute(8)
    Actor.receive { case d => }
    execute(32)
  }
}
