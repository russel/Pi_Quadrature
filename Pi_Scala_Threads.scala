/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import scala.concurrent.SyncVar

object Pi_Scala_Threads extends  Application {
  def execute ( numberOfTasks : Int ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfTasks
    var sum = new SyncVar[Double]
    sum set 0.0
    var threads = for ( index <- 0 until numberOfTasks ) yield
      new Thread ( new Runnable {
        def run ( ) {
          var i = 1 + index * sliceSize
          val end = ( index + 1 ) * sliceSize
          var localSum = 0.0
          while ( i <= end ) {
            val x = ( i - 0.5 ) * delta
            localSum += 1.0 / ( 1.0 + x * x )
            i += 1
          }
          sum set sum.get + localSum
        }
      } )
    threads.foreach ( t => t.start )
    threads.foreach ( t => t.join )
    val pi = 4.0 * sum.get * delta
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
