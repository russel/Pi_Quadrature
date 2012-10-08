/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using an actor system.
 *
 *  Copyright © 2011 SarahMount
 *  Copyright © 2011–2012 Russel Winder
 */

//  This variant initially coded up by Sarah Mount then heavily amended by Russel Winder.

import scala.actors.Actor

import SOutput.out

class Calculator(id:Int, sliceSize:Int, delta:Double, accumulator:Actor) extends Actor {
  def act {
    var sum = 0.0
    var x  = 0.0
    for (i <- 1 + id * sliceSize to (id + 1) * sliceSize + 1) {
      x = (i - 0.5) * delta
      sum += 1.0 / (1.0 + x * x)
    }
    accumulator ! sum
  }
}

class Accumulator(numberOfActors:Int, delta:Double) extends Actor {
  var sum = 0.0
  def act {
    receive {
      case i:Int =>
        for (i <- 0 until numberOfActors) {
          receive {
	    case d:Double =>
    	      sum += d.asInstanceOf[Double]
          }
        }
        reply(4.0 * delta * sum)
    }
  }
}

object Pi_Scala_Actors_SM {
  def execute(numberOfActors:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfActors
    val accumulator = new Accumulator(numberOfActors, delta)
    accumulator.start()
    accumulator ! 0
    val calculators = for (i <- 0 until numberOfActors) yield new Calculator(i, sliceSize, delta, accumulator)
    calculators.foreach(_.start())
    Actor.receive {
      case pi:Double =>
        val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
        out("Pi_Scala_Actors_SM", pi, n, elapseTime, numberOfActors)
    }
  }
  def main(args:Array[String]) {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }
}
