#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars CSP to partition the
 *  problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess

import groovyx.gpars.csp.PAR

void execute ( final int numberOfTasks ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final channel = Channel.any2one ( )
  final processes = [ ]
  for ( int i in 0i ..< numberOfTasks ) { 
    final int taskId = i
    processes << new CSProcess ( ) {
      @Override public void run ( ) {
        final int start = 1i + taskId * sliceSize
        final int end = ( taskId + 1i ) * sliceSize
        double sum = 0.0 ;
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
      println ( '==== Groovy GPars CSP Single pi = ' + pi )
      println ( '==== Groovy GPars CSP Single iteration count = ' + n )
      println ( '==== Groovy GPars CSP Single elapse = ' + elapseTime )
      println ( '==== Groovy GPars CSP Single processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
      println ( '==== Groovy GPars CSP Single task count = ' + numberOfTasks )
    }
  } ;
  ( new PAR ( processes ) ).run ( )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
