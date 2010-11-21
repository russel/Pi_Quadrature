#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars CSP to partition the
 *  problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010 Russel Winder
 */

@Grab ( group = 'org.codehaus.jcsp' , module = 'jcsp' , version = '1.1-rc5' )
@Grab ( group = 'org.codehaus.gpars' , module = 'gpars' , version = '0.11-beta-1' )

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess

import groovyx.gpars.csp.PAR

void execute ( final int numberOfTasks ) {
  final long n = 1000000000l
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final long sliceSize = n / numberOfTasks
  final channel = Channel.any2one ( )
  final processes = [ ]
  for ( i in 0 ..< numberOfTasks ) { processes << new ProcessSlice_JCSP ( i , sliceSize , delta , channel.out ( ) ) }
  processes << new CSProcess ( ) {
    public void run ( ) {
      double sum = 0.0d
      for ( i in 0 ..< numberOfTasks ) { sum += (double) channel.in ( ).read ( ) }
      final double pi = 4.0d * sum * delta
      final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      println ( "==== Groovy/Java GPars CSP Single pi = " + pi )
      println ( "==== Groovy/Java GPars CSP Single iteration count = " + n )
      println ( "==== Groovy/Java GPars CSP Single elapse = " + elapseTime )
      println ( "==== Groovy/Java GPars CSP Single processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) )
      println ( "==== Groovy/Java GPars CSP Single task count = " + numberOfTasks )
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
