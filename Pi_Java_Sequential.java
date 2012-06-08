/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

public class Pi_Java_Sequential {
  public static void main ( final String[] args ) {
    final int n = 1000000000 ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    double sum = 0.0 ;
    for ( int i = 1 ; i <= n ; ++i ) {
      final double x = ( i - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
    final double pi = 4.0 * delta * sum ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    JOutput.out ( "Java Sequential" , pi , n , elapseTime ) ;
  }
}
