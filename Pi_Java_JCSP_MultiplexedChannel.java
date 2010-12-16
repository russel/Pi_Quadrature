/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with JCSP to partition the problem
 *  and hence harness all processors available to the JVM.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import org.jcsp.lang.Channel ;
import org.jcsp.lang.CSProcess ;
import org.jcsp.lang.Any2OneChannel ;
import org.jcsp.lang.Parallel ;

import java.util.ArrayList ;
import java.util.Arrays ;
import java.util.List ;

public class Pi_Java_JCSP_MultiplexedChannel {
  private static void execute ( final int numberOfTasks ) {
    final long n = 1000000000l ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final long sliceSize = n / numberOfTasks ;
    final Any2OneChannel<Double> channel = Channel.any2one ( ) ;
    final List<CSProcess> processes = new ArrayList<CSProcess> ( ) ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int taskId = i ;
      processes.add ( new CSProcess ( ) {
          @Override public void run ( ) {
            final long start = 1 + taskId * sliceSize ;
            final long end = ( taskId + 1 ) * sliceSize ;
            double sum = 0.0 ;
            for ( long i = start ; i <= end ; ++i ) {
              final double x = ( i - 0.5 ) * delta ;
              sum += 1.0 / ( 1.0 + x * x ) ;
            }
            channel.out ( ).write ( sum ) ;
          }
        } ) ;
    }
    processes.add ( new CSProcess ( ) {
        @Override public void run ( ) {
          double sum = 0.0 ;
          for ( int i = 0 ; i < numberOfTasks ; ++i ) {  sum += channel.in ( ).read ( ) ; }
          final double pi = 4.0 * sum * delta ;
          final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
          System.out.println ( "==== Java JCSP MultiplexedChannel pi = " + pi ) ;
          System.out.println ( "==== Java JCSP MultiplexedChannel iteration count = " + n ) ;
          System.out.println ( "==== Java JCSP MultiplexedChannel elapse = " + elapseTime ) ;
          System.out.println ( "==== Java JCSP MultiplexedChannel processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
          System.out.println ( "==== Java JCSP MultiplexedChannel task count = " + numberOfTasks ) ;
        }
      } ) ;
    ( new Parallel ( processes.toArray ( new CSProcess [0] ) ) ).run ( ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_JCSP_MultiplexedChannel.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_JCSP_MultiplexedChannel.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_JCSP_MultiplexedChannel.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_JCSP_MultiplexedChannel.execute ( 32 ) ;
  }
}
