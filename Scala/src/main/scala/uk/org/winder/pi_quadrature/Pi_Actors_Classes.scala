/*
 *  Calculation of π using quadrature realized with a scatter/gather approach using an actor system.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

import akka.actor.{Actor, ActorSystem, Props}

import Output.out

object Pi_Actors_Classes extends App {

  implicit val system = ActorSystem("PiActorsClasses")

  def execute(numberOfWorkerActors:Int) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkerActors
    val accumulator = system.actorOf(Props(new Actor {
      var sum = 0.0
      var count = 0
      def receive = {
        case d:Double =>
          sum += d
          count += 1
          if (count == numberOfWorkerActors) {
            val pi = 4.0 * delta * sum
            val elapseTime = (System.nanoTime - startTimeNanos) / 1e9
            out("Pi_Actors_Classes", pi, n, elapseTime, numberOfWorkerActors)
            sequencer ! 0
          }
      }
    }))
    for (index <- 0 until numberOfWorkerActors) {
      system.actorOf(Props(new Actor {
        override def preStart = {
          val start = 1 + index * sliceSize
          val end = (index + 1) * sliceSize
          var sum = 0.0
          for (i <- start to end) {
            val x = (i - 0.5) * delta
            sum += 1.0 / (1.0 + x * x)
          }
          accumulator ! sum
       }
       def receive = {
         case o:Object =>
       }
      }))
    }
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

  sequencer ! 0

}
