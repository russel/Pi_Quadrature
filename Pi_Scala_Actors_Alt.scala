/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using an actor system.
 * 
 *  Copyright © 2009–2011 Russel Winder
 */

//  Version using explicit Actor classes rather than using the Actors.actor factory method.  cf.  Pi_Scala_Actors.scala.

//  TODO: Investigate why there is a slow-down using eight or 32 actors on a dual-core compared to
//  using two actors?

import scala.actors.Actor

class Accumulator ( numberOfWorkerActors : Int , delta : Double , n : Int , startTimeNanos : Long , sequencer : Actor ) extends Actor {
  def act ( ) {
    var sum = 0.0
    for ( i <- 0 until numberOfWorkerActors ) { Actor.receive { case d => sum += d.asInstanceOf[Double] } }
    val pi = 4.0 * delta * sum
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Actors Alt pi = " + pi )
    println ( "==== Scala Actors Alt iteration count = " + n )
    println ( "==== Scala Actors Alt elapse = " + elapseTime )
    println ( "==== Scala Actors Alt processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Actors Alt worker actor count = " + numberOfWorkerActors )
    sequencer ! 0
  }
}

class Calculator ( id : Int , sliceSize : Int , delta : Double , accumulator : Actor ) extends Actor {
  def act ( ) {
    val start = 1 + id * sliceSize
    val end = ( id + 1 ) * sliceSize 
    var sum = 0.0
    for ( i <- start to end ) {
      val x = ( i - 0.5 ) * delta
      sum += 1.0 / ( 1.0 + x * x )
    }
    accumulator ! sum
  }
}

object Pi_Scala_Actors_Alt extends App {
  def execute ( numberOfWorkerActors : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkerActors
    val accumulator = new Accumulator ( numberOfWorkerActors , delta , n , startTimeNanos , sequencer )
    accumulator.start ( )
    val calculators = new Array[Actor] ( numberOfWorkerActors )
    for ( id <- calculators.indices ) {
      calculators ( id ) = new Calculator ( id , sliceSize , delta , accumulator )
      calculators ( id ).start ( )
    }
  }
  val sequencer = Actor.actor {
    execute ( 1 )
    Actor.receive { case d => }
    println
    execute ( 2 )
    Actor.receive { case d => }
    println
    execute ( 8 )
    Actor.receive { case d => }
    println
    execute ( 32 )
  }
}
