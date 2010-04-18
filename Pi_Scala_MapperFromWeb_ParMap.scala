/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import scala.actors.Futures

object Pi_Scala_MapperFromWeb_ParMap extends Application {
  class Mapper[A,B:ClassManifest] ( data : List[A] , function : A => B ) {
    val pmap = {
      val buffer = new Array[B] ( data.length )
      val mappers = for ( index <- 0 until data.length ) yield Futures.future { buffer ( index ) = function ( data ( index ) ) }
      for ( mapper <- mappers ) mapper ( ) // NB This set of parentheses is essential.
      buffer
    }
  }
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000l
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = ( index : Int ) => {
      var i = 1 + index * sliceSize
      val end = ( index + 1 ) * sliceSize 
      var sum = 0.0
      while ( i <=  end ) {
        val x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
        i += 1
      }
      sum
    }
    //  Why the "wrong number of arguments for method apply" error if we put () after the pmap?
    //  How to choose between reduceRight and reduceLeft?
    val sum = ( ( new Mapper[Int,Double] ( List.range ( 0 , numberOfThreads ) , partialSum ) ).pmap ) reduceLeft ( _ + _ )
    val pi = 4.0 * sum * delta
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala Parallel Map pi = " + pi )
    println ( "==== Scala Parallel Map iteration count = " + n )
    println ( "==== Scala Parallel Map elapse = " + elapseTime )
    println ( "==== Scala Parallel Map processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala Parallel Map threads count = " + numberOfThreads )
  }
  execute ( 1 )
  println
  execute ( 2 )
  println
  execute ( 8 )
  println
  execute ( 32 )
}
