/*
 *  Calculation of π using quadrature realized with a scatter/gather approach using an actor system.
 *
 *  Copyright © 2011  SarahMount
 *  Copyright © 2011–2013  Russel Winder
 */

//  This variant initially coded up by Sarah Mount then heavily amended by Russel Winder.

package uk.org.winder.pi_quadrature

import akka.actor.{Actor, ActorRef, ActorSystem}
import akka.actor.Props

import Output.out

object Pi_Actors_SM {

	implicit val system = ActorSystem("PiActorsSM")

	class Calculator(id:Int, sliceSize:Int, delta:Double, accumulator:ActorRef) extends Actor {
		override def preStart {
			var sum = 0.0
			var x  = 0.0
			for (i <- 1 + id * sliceSize to (id + 1) * sliceSize + 1) {
				x = (i - 0.5) * delta
				sum += 1.0 / (1.0 + x * x)
			}
			accumulator ! sum
		}
		def receive = { case o:Object => }
	}

	class Accumulator(numberOfActors:Int, n:Int, delta:Double, startTimeNanos:Double) extends Actor {
		var sum = 0.0
		var i = 0
		def receive = {
			case d:Double =>
				sum += d.asInstanceOf[Double]
				i += 1
				if (i == numberOfActors) {
					val pi = 4.0 * delta * sum
					val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
					out("Pi_Actors_SM", pi, n, elapseTime, numberOfActors)
					sequencer ! 0
				}
		}
	}

	def execute(numberOfActors:Int) {
		val n = 1000000000
		val delta = 1.0 / n
		val startTimeNanos = System.nanoTime
		val sliceSize = n / numberOfActors
		val accumulator = system.actorOf(Props(new Accumulator(numberOfActors, n, delta, startTimeNanos)))
		for (i <- 0 until numberOfActors) yield system.actorOf(Props(new Calculator(i, sliceSize, delta, accumulator)))
	}

	val sequencer = system.actorOf(Props(new Actor {
		val numbers = List(1, 2, 8, 32)
		var index = -1
		def receive = {
			case i:Int =>
				index += 1
				if (index == numbers.size) { system.terminate }
				else { execute(numbers(index)) }
		}
	}))

	def main(args:Array[String]) {
		sequencer ! 0
	}

}
