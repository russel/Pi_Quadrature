#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars CSP to partition the
 *  problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2011 Russel Winder
 */

@Grab ( 'org.codehaus.jcsp:jcsp:1.1-rc5' )
@Grab ( 'org.codehaus.gpars:gpars:1.0-SNAPSHOT' )

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess

import groovyx.gpars.csp.PAR

void execute ( final int numberOfTasks ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final channel = Channel.any2one ( )
  final processes = [ ]
  for ( int i in 0i ..< numberOfTasks ) { processes << new ProcessSlice_JCSP ( i , sliceSize , delta , channel.out ( ) ) }
  processes << new CSProcess ( ) {
    @Override public void run ( ) {
      final double pi = 4.0d * delta * ( 0i ..< numberOfTasks ).sum { (double) channel.in ( ).read ( ) }
      final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      println ( '==== Groovy/Java GPars CSP Single pi = ' + pi )
      println ( '==== Groovy/Java GPars CSP Single iteration count = ' + n )
      println ( '==== Groovy/Java GPars CSP Single elapse = ' + elapseTime )
      println ( '==== Groovy/Java GPars CSP Single processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
      println ( '==== Groovy/Java GPars CSP Single task count = ' + numberOfTasks )
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
