/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with the ForkJoin library of
 *  JSR166y that is destined to be a part of Java 7.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

import java.util.concurrent.Callable ;
import java.util.concurrent.ExecutionException ;
import jsr166y.ForkJoinPool ;
import jsr166y.ForkJoinTask ;

public class Pi_Java_ForkJoinBasic {
  private static void execute ( final int numberOfTasks ) {
    final int n = 1000000000 ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final int sliceSize = n / numberOfTasks ;
    final ForkJoinPool pool = new ForkJoinPool ( numberOfTasks ) ;
    @SuppressWarnings ( "unchecked" ) final ForkJoinTask<Double>[] futures = new ForkJoinTask [ numberOfTasks ] ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int taskId = i ;
      futures[taskId] = pool.submit ( new Callable<Double> ( ) {
          @Override public Double call ( ) {
            final int start = 1 + taskId * sliceSize ;
            final int end = ( taskId + 1 ) * sliceSize ;
            double sum = 0.0 ;
            for ( int i = start ; i <= end ; ++i ) {
              final double x = ( i - 0.5 ) * delta ;
              sum += 1.0 / ( 1.0 + x * x ) ;
            }
            return sum ;
          }
        } ) ;
    }
    double sum = 0.0 ;
    for ( ForkJoinTask<Double> f : futures ) {
      try { sum += f.get ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( ie ) ; } 
      catch ( final ExecutionException ee ) { throw new RuntimeException ( ee ) ; } 
    }
    final double pi = 4.0 * sum * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    pool.shutdown ( ) ;
    System.out.println ( "==== Java ForkJoin Basic pi = " + pi ) ;
    System.out.println ( "==== Java ForkJoin Basic iteration count = " + n ) ;
    System.out.println ( "==== Java ForkJoin Basic elapse = " + elapseTime ) ;
    System.out.println ( "==== Java ForkJoin Basic processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "==== Java ForkJoin Basic thread count = " + numberOfTasks ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_ForkJoinBasic.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_ForkJoinBasic.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_ForkJoinBasic.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_ForkJoinBasic.execute ( 32 ) ;
  }
}
