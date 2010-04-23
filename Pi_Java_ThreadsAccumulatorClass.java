/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009-10 Russel Winder
 */

public class Pi_Java_ThreadsAccumulatorClass {
  private static class Accumulator {
    private double sum = 0.0 ;
    public synchronized void add ( final double value ) { sum += value ; }
    public synchronized double getSum ( ) { return sum ; }
  }
  private static void execute ( final int numberOfTasks ) {
    final long n = 1000000000l ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final long sliceSize = n / numberOfTasks ;
    final Thread[] threads = new Thread [ numberOfTasks ] ;
    final Accumulator accumulator = new Accumulator ( ) ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int id = i ;
      threads[id] = new Thread ( new Runnable ( ) {
          public void run ( ) {
            final long start = 1 + id * sliceSize ;
            final long end = ( id + 1 ) * sliceSize ;
            double sum = 0.0 ;
            for ( long i = start ; i <= end ; ++i ) {
              final double x = ( i - 0.5 ) * delta ;
              sum += 1.0 / ( 1.0 + x * x ) ;
            }
            accumulator.add ( sum ) ;
          }
        } ) ;
    }
    for ( final Thread t : threads ) { t.start ( ) ; }
    for ( final Thread t : threads ) {
      try { t.join ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( "Got an InterruptedException joining a thread." , ie ) ; }
    }
    final double pi = 4.0 * accumulator.getSum ( ) * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    System.out.println ( "==== Java Threads Accumulator Class pi = " + pi ) ;
    System.out.println ( "==== Java Threads Accumulator Class iteration count = " + n ) ;
    System.out.println ( "==== Java Threads Accumulator Class elapse = " + elapseTime ) ;
    System.out.println ( "==== Java Threads Accumulator Class processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "==== Java Threads Accumulator Class thread count = " + numberOfTasks ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_ThreadsAccumulatorClass.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_ThreadsAccumulatorClass.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_ThreadsAccumulatorClass.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_ThreadsAccumulatorClass.execute ( 32 ) ;
  }
}
