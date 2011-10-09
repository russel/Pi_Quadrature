/*
 *  Calculation of Pi using quadrature realized with a parallel map.
 *
 *  Copyright © 2009–2010 Russel Winder
 */

import scala.actors.Futures

object Pi_Scala_MapperFromWeb_ParMap {
  class Mapper[A,B:ClassManifest] ( data : List[A] , function : A => B ) {
    val pmap = {
      val buffer = new Array[B] ( data.length )
      val mappers = for ( index <- 0 until data.length ) yield Futures.future { buffer ( index ) = function ( data ( index ) ) }
      for ( mapper <- mappers ) mapper ( ) // NB This set of parentheses is essential.
      buffer
    }
  }
  def execute ( numberOfThreads : Int ) {
    val n = 1000000000
    val delta = 1.0 / n
    val startTimeNanos = System.nanoTime
    val sliceSize = n / numberOfThreads
    val partialSum = ( index : Int ) => {
      val start = 1 + index * sliceSize
      val end = ( index + 1 ) * sliceSize 
      var sum = 0.0
      for ( i <- start to end ) {
        val x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
      }
      sum
    }
    val pi = 4.0 * delta * ( ( ( new Mapper[Int,Double] ( List.range ( 0 , numberOfThreads ) , partialSum ) ).pmap ) reduceLeft ( _ + _ ) )
    val elapseTime = ( System.nanoTime - startTimeNanos ) / 1e9
    println ( "==== Scala MapperFromWeb ParMap pi = " + pi )
    println ( "==== Scala MapperFromWeb ParMap iteration count = " + n )
    println ( "==== Scala MapperFromWeb ParMap elapse = " + elapseTime )
    println ( "==== Scala MapperFromWeb ParMap processor count = " + Runtime.getRuntime.availableProcessors )
    println ( "==== Scala MapperFromWeb ParMap threads count = " + numberOfThreads )
  }
  def main ( args : Array[String] ) {
    execute ( 1 )
    println
    execute ( 2 )
    println
    execute ( 8 )
    println
    execute ( 32 )
  }
}
