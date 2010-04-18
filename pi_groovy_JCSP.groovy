#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with JCSP to partition the problem
 *  and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010 Russel Winder
 */

@Grab ( group = 'org.codehaus.jcsp' , module = 'jcsp' , version = '1.1-rc5-SNAPSHOT' )

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess
import org.jcsp.lang.Parallel

void execute ( int numberOfTasks ) {
  final long n = 100000000l // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final long startTimeNanos = System.nanoTime ( )
  final long sliceSize = n / numberOfTasks
  final channels = Channel.one2oneArray ( numberOfTasks )
  final processes = [ ]
  for ( int i = 0 ; i < numberOfTasks ; ++i ) {
    final int taskId = i
    processes <<  new CSProcess ( ) {
      public void run ( ) {
        double sum = 0.0 ;
        final long start = 1 + taskId * sliceSize
        final long end = ( taskId + 1 ) * sliceSize
        for ( long j = start ; j <= end ; ++j ) {
          final double x = ( j - 0.5d ) * delta
          sum += 1.0d / ( 1.0d + x * x )
        }
        channels[taskId].out ( ).write ( sum )
      }
    }
  }
  processes << new CSProcess ( ) {
    public void run ( ) {
      double sum = 0.0d
      for ( c in channels ) { sum += (double) c.in ( ).read ( ) }
      final double pi = 4.0d * sum * delta
      final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      System.out.println ( "==== Groovy JCSP pi = " + pi )
      System.out.println ( "==== Groovy JCSP iteration count = " + n )
      System.out.println ( "==== Groovy JCSP elapse = " + elapseTime )
      System.out.println ( "==== Groovy JCSP processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) )
      System.out.println ( "==== Groovy JCSP task count = " + numberOfTasks )
    }
  } ;
  ( new Parallel ( processes as CSProcess[] ) ).run ( )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
