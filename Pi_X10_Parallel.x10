/*
 *  Calculation of Pi using quadrature realized with a data parallel algorithm.
 *
 *  As at versions 2.0.[23], X10 fails to terminate compilation in the presence of the copyright symbol as a
 *  UTF-8 encoded Unicode codepoint.  See XTENLANG-1236, http://jira.codehaus.org/browse/XTENLANG-1236.
 *
 *  Copyright (c) 2009-10 Russel Winder
 */

import x10.io.Console ;

public class Pi_X10_Parallel {
  private static def execute ( numberOfTasks : int ) : void {
    val n : long = 1000000000l ;
    val delta : double = 1.0 / n ;
    val startTimeNanos : long = System.nanoTime ( ) ;
    val sliceSize : long = n / numberOfTasks ;
    val computeSlice = ( p : Point ) => {
      val id : int = p(0) ;
      val start : long = 1 + id * sliceSize ;
      val end : long = ( id + 1 ) * sliceSize ;
      var sum : double = 0.0 ;
      for ( var i : long = start ; i < end ; ++i ) {
        val x : double = ( i - 0.5 ) * delta ;
        sum += 1.0 / ( 1.0 + x * x ) ;
      }
      sum
    } ;
    //  This operation is map not lift.
    val sums = ( new Array[Double] ( numberOfTasks) ).lift ( computeSlice ) ;
    val pi : double = 4.0 * sums.reduce ( Double.+ , 0.0 ) * delta ;
    val elapseTime : double = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    Console.OUT.println ( "==== X10 Sequential pi = " + pi ) ;
    Console.OUT.println ( "==== X10 Sequential iteration count = " + n ) ;
    Console.OUT.println ( "==== X10 Sequential elapse = " + elapseTime ) ;
  }
  public static def main ( args : Rail[String] ! ) : void {
    execute ( 1 ) ;
    Console.OUT.println ( ) ;
    execute ( 2 ) ;
    Console.OUT.println ( ) ;
    execute ( 8 ) ;
    Console.OUT.println ( ) ;
    execute ( 32 ) ;    
  }
}
