/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with the ForkJoin library of
 *  JSR166y that is destined to be a part of Java 7.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

import java.util.ArrayList ;

import java.util.concurrent.Callable ;
import java.util.concurrent.ExecutionException ;
import java.util.concurrent.Future ;
import jsr166y.ForkJoinPool ;

public class Pi_Java_ForkJoinCollection {
  private static void execute ( final int numberOfTasks ) {
    final int n = 1000000000 ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final int sliceSize = n / numberOfTasks ;
    final ForkJoinPool executor = new ForkJoinPool ( numberOfTasks ) ;
    final ArrayList<Callable<Double>> callables = new ArrayList<Callable<Double>> ( ) ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int taskId = i ;
      callables.add ( new Callable<Double> ( ) {
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
    for ( Future<Double> f : executor.invokeAll ( callables ) ) {
      try { sum += f.get ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( ie ) ; } 
      catch ( final ExecutionException ee ) { throw new RuntimeException ( ee ) ; } 
    }
    final double pi = 4.0 * sum * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    executor.shutdown ( ) ;
    System.out.println ( "==== Java ForkJoin Collection pi = " + pi ) ;
    System.out.println ( "==== Java ForkJoin Collection iteration count = " + n ) ;
    System.out.println ( "==== Java ForkJoin Collection elapse = " + elapseTime ) ;
    System.out.println ( "==== Java ForkJoin Collection processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "==== Java ForkJoin Collection thread count = " + numberOfTasks ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_ForkJoinCollection.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_ForkJoinCollection.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_ForkJoinCollection.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_ForkJoinCollection.execute ( 32 ) ;
  }
}
