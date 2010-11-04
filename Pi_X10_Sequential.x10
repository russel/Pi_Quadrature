/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  As at versions 2.0.[23], X10 fails to terminate compilation in the presence of the copyright symbol as a
 *  UTF-8 encoded Unicode codepoint.  See XTENLANG-1236, http://jira.codehaus.org/browse/XTENLANG-1236.
 *  Version 2.1.0 terminates but gives an error message of bizarre nature.
 *
 *  Copyright (c) 2009-10 Russel Winder
 */

import x10.io.Console ;

public class Pi_X10_Sequential {
  public static def main ( args : Array[String] ) : void {
    val n : long = 1000000000l ;
    val delta : double = 1.0 / n ;
    val startTimeNanos : long = System.nanoTime ( ) ;
    var sum : double = 0.0 ;
    for ( var i : long = 1 ; i <= n ; ++i ) {
      val x : double = ( i - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
    val pi : double = 4.0 * sum * delta ;
    val elapseTime : double = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    Console.OUT.println ( "==== X10 Sequential pi = " + pi ) ;
    Console.OUT.println ( "==== X10 Sequential iteration count = " + n ) ;
    Console.OUT.println ( "==== X10 Sequential elapse = " + elapseTime ) ;
  }
}
