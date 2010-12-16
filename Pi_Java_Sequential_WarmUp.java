/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *  Show the JVM warm up by executing the same thing a number of times.
 * 
 *  Copyright © 2008-10 Russel Winder
 */

public class Pi_Java_Sequential_WarmUp {
  public static void execute ( ) {
    final long n = 1000000000l ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    double sum = 0.0 ;
    for ( long i = 1 ; i <= n ; ++i ) {
      final double x = ( i - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
    final double pi = 4.0 * sum * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    System.out.println ( "==== Java Sequential Warm Up pi = " + pi ) ;
    System.out.println ( "==== Java Sequential Warm Up iteration count = " + n ) ;
    System.out.println ( "==== Java Sequential Warm Up elapse = " + elapseTime ) ;
  }
  public static void main ( final String[] args ) {
    execute ( ) ;
    System.err.println ( ) ;
    execute ( ) ;
    System.err.println ( ) ;
    execute ( ) ;
    System.err.println ( ) ;
    execute ( ) ;
  }
}
