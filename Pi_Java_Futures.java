/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with threads and futures (hidden
 *  by using executors) to partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2008-10 Russel Winder
 */

import java.util.ArrayList ;

import java.util.concurrent.Callable ;
import java.util.concurrent.ExecutorService ;
import java.util.concurrent.ExecutionException ;
import java.util.concurrent.Future ;
import java.util.concurrent.ScheduledThreadPoolExecutor ;

public class Pi_Java_Futures {
  private static void execute ( final int numberOfTasks ) {
    final long n = 1000000000l ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final long sliceSize = n / numberOfTasks ;
    final ExecutorService executor = new ScheduledThreadPoolExecutor ( numberOfTasks ) ;
    final ArrayList<Future<Double>> futures = new ArrayList<Future<Double>> ( ) ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int id = i ;
      futures.add ( executor.submit ( new Callable<Double> ( ) {
            @Override public Double call ( ) {
              final long start = 1 + id * sliceSize ;
              final long end = ( id + 1 ) * sliceSize ;
              double sum = 0.0 ;
              for ( long i = start ; i <= end ; ++i ) {
                final double x = ( i - 0.5 ) * delta ;
                sum += 1.0 / ( 1.0 + x * x ) ;
              }
              return sum ;
            }
          } ) ) ;
    }
    double sum = 0.0 ;
    for ( Future<Double> f : futures ) {
      try { sum += f.get ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( ie ) ; } 
      catch ( final ExecutionException ee ) { throw new RuntimeException ( ee ) ; } 
    }
    final double pi = 4.0 * sum * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    executor.shutdown ( ) ;
    System.out.println ( "==== Java Futures pi = " + pi ) ;
    System.out.println ( "==== Java Futures iteration count = " + n ) ;
    System.out.println ( "==== Java Futures elapse = " + elapseTime ) ;
    System.out.println ( "==== Java Futures processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "==== Java Futures thread count = " + numberOfTasks ) ;
  }
  public static void main ( final String[] args ) {
    //  It seems we have to run the function twice to warm up the JVM :-(
    //Pi_Java_Futures.execute ( 8 ) ;
    //System.out.println ( ) ;
    //Pi_Java_Futures.execute ( 8 ) ;
    //System.out.println ( ) ;
    Pi_Java_Futures.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_Futures.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_Futures.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_Futures.execute ( 32 ) ;
  }
}
