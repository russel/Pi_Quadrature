/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using an actor system.
 * 
 *  Copyright © 2009--2011 Russel Winder
 */

//  TODO: Investigate why there is a slow-down using eight actors on a dual-core machine compared to using
//  two or 32 actors?

import scala.actors.Actor

object Pi_Scala_Actors extends Application {
  def execute ( numberOfWorkerActors : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfWorkerActors
    val calculators = new Array[Actor] ( numberOfWorkerActors )
    val accumulator = Actor.actor {
      var sum = 0.0
      calculators.foreach ( calculator => Actor.receive { case d => sum += d.asInstanceOf[Double] } )
      val pi = 4.0 * delta * sum
      val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
      println ( "==== Scala Actors pi = " + pi )
      println ( "==== Scala Actors iteration count = " + n )
      println ( "==== Scala Actors elapse = " + elapseTime )
      println ( "==== Scala Actors processor count = " + Runtime.getRuntime.availableProcessors )
      println ( "==== Scala Actors worker actor count = " + numberOfWorkerActors )
      sequencer ! 0
    }
    for ( index <- 0 until calculators.size ) {
      calculators ( index ) = Actor.actor {
        val start = 1 + index * sliceSize
        val end = ( index + 1 ) * sliceSize 
        var sum = 0.0
        for ( i <- start to end ) {
          val x = ( i - 0.5 ) * delta
          sum += 1.0 / ( 1.0 + x * x )
        }
        accumulator ! sum
      }
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
