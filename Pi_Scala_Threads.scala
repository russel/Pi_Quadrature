/*
 *  Calculation of Pi using quadrature realized with a scatter/gather approach using threads.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

import scala.concurrent.Lock

object Pi_Scala_Threads extends  App {
  def execute ( numberOfTasks : Int ) = {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfTasks
    var sum = 0.0
    val lock = new Lock ( )
    var threads = for ( index <- 0 until numberOfTasks ) yield
      new Thread ( new Runnable {
        def run ( ) {
          val start = 1 + index * sliceSize
          val end = ( index + 1 ) * sliceSize
          var localSum = 0.0
          for ( i <- start to end ) {
            val x = ( i - 0.5 ) * delta
            localSum += 1.0 / ( 1.0 + x * x )
          }
          lock.acquire
          sum += localSum
          lock.release
        }
      } )
    threads.foreach ( t => t.start )
    threads.foreach ( t => t.join )
    val pi = 4.0 * delta * sum
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Threads pi = " + pi )
    println ( "==== Scala Threads iteration count = " + n )
    println ( "==== Scala Threads elapse = " + elapseTime )
    println ( "==== Scala Threads processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Threads thread count = " + numberOfTasks )
  }
  execute ( 1 )
  println
  execute ( 2 )
  println
  execute ( 8 )
  println
  execute ( 32 )
}
