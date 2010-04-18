/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009 Russel Winder
 */

public class Pi_Java_ThreadsSynchronizedStatement {
  private static Double sum ; // Must be non-primitive! (The object lock of this object is used for synchronization.)
  private static void execute ( final int numberOfTasks ) {
    final long n = 1000000000l ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final long sliceSize = n / numberOfTasks ;
    final Thread[] threads = new Thread [ numberOfTasks ] ;
    sum = 0.0 ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int id = i ;
      threads[id] = new Thread ( new Runnable ( ) {
          public void run ( ) {
            final long start = 1 + id * sliceSize ;
            final long end = ( id + 1 ) * sliceSize ;
            double localSum = 0.0 ;
            for ( long i = start ; i <= end ; ++i ) {
              final double x = ( i - 0.5 ) * delta ;
              localSum += 1.0 / ( 1.0 + x * x ) ;
            }
            synchronized ( sum ) { sum += localSum ; }
          }
        } ) ;
    }
    for ( final Thread t : threads ) { t.start ( ) ; }
    for ( final Thread t : threads ) {
      try { t.join ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( ie ) ; }
    }
    final double pi = 4.0 * sum * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    System.out.println ( "==== Java Threads Synchronized Statement pi = " + pi ) ;
    System.out.println ( "==== Java Threads Synchronized Statement iteration count = " + n ) ;
    System.out.println ( "==== Java Threads Synchronized Statement elapse = " + elapseTime ) ;
    System.out.println ( "==== Java Threads Synchronized Statement processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "==== Java Threads Synchronized Statement thread count = " + numberOfTasks ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_ThreadsSynchronizedStatement.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_ThreadsSynchronizedStatement.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_ThreadsSynchronizedStatement.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_ThreadsSynchronizedStatement.execute ( 32 ) ;
  }
}
