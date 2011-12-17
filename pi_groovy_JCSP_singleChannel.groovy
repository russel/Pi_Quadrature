#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with JCSP to partition the problem
 *  and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2011 Russel Winder
 */

@Grab ( group = 'org.codehaus.jcsp' , module = 'jcsp' , version = '1.1-rc5' )

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess
import org.jcsp.lang.Parallel

void execute ( int numberOfTasks ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final channel = Channel.any2one ( )
  final processes = [ ]
  for ( int i in 0i ..< numberOfTasks ) {
    final int taskId = i
    processes <<  new CSProcess ( ) {
      @Override public void run ( ) {
        final int start = 1i + taskId * sliceSize
        final int end = ( taskId + 1i ) * sliceSize
        double sum = 0.0
        for ( int j in start .. end ) {
          final double x = ( j - 0.5d ) * delta
          sum += 1.0d / ( 1.0d + x * x )
        }
        channel.out ( ).write ( sum )
      }
    }
  }
  processes << new CSProcess ( ) {
    @Override public void run ( ) {
      double sum = 0.0d
      for ( int i in 0i ..< numberOfTasks ) { sum += (double) channel.in ( ).read ( ) }
      final double pi = 4.0d * delta * sum
      final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      println ( "==== Groovy JCSP Single pi = " + pi )
      println ( "==== Groovy JCSP Single iteration count = " + n )
      println ( "==== Groovy JCSP Single elapse = " + elapseTime )
      println ( "==== Groovy JCSP Single processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) )
      println ( "==== Groovy JCSP Single task count = " + numberOfTasks )
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
