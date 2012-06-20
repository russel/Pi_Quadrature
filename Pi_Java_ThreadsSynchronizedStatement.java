/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

public class Pi_Java_ThreadsSynchronizedStatement {
  //  A singleton constant object used for all the synchronization.  As pointed out by Roger Orr (private
  //  communication, 2010-04-19) there is a subtle bug in using the sum variable (if it were object) to synchronize
  //  actions on itself since the variable refers to a different object after the update operation compared
  //  to the one it refers to when the object lock is claimed.
  private final static Integer lockObject = 1 ;
  private static double sum ;
  private static void execute ( final int numberOfTasks ) {
    final int n = 1000000000 ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final int sliceSize = n / numberOfTasks ;
    final Thread[] threads = new Thread [ numberOfTasks ] ;
    sum = 0.0 ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int taskId = i ;
      threads[taskId] = new Thread ( new Runnable ( ) {
          @Override public void run ( ) {
            final int start = 1 + taskId * sliceSize ;
            final int end = ( taskId + 1 ) * sliceSize ;
            double localSum = 0.0 ;
            for ( int i = start ; i <= end ; ++i ) {
              final double x = ( i - 0.5 ) * delta ;
              localSum += 1.0 / ( 1.0 + x * x ) ;
            }
            synchronized ( lockObject ) { sum += localSum ; }
          }
        } ) ;
    }
    for ( final Thread t : threads ) { t.start ( ) ; }
    for ( final Thread t : threads ) {
      try { t.join ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( "Got an InterruptedException joining a thread." , ie ) ; }
    }
    final double pi = 4.0 * delta * sum ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    JOutput.out ( "Pi_Java_ThreadsSynchronized Statement" , pi, n , elapseTime , numberOfTasks ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_ThreadsSynchronizedStatement.execute ( 1 ) ;
    Pi_Java_ThreadsSynchronizedStatement.execute ( 2 ) ;
    Pi_Java_ThreadsSynchronizedStatement.execute ( 8 ) ;
    Pi_Java_ThreadsSynchronizedStatement.execute ( 32 ) ;
  }
}
