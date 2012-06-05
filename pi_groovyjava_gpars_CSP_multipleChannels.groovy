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
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final channels = Channel.one2oneArray ( numberOfTasks )
  final processes = [ ]
  for ( int i in 0i ..< numberOfTasks ) { processes << new ProcessSlice_JCSP ( i , sliceSize , delta , channels[i].out ( ) ) }
  processes << new CSProcess ( ) {
    @Override public void run ( ) {
      final double pi = 4.0d * delta * channels.sum { c -> (double) c.in ( ).read ( ) }
      final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      println ( '==== Groovy/Java GPars CSP Multiple pi = ' + pi )
      println ( '==== Groovy/Java GPars CSP Multiple iteration count = ' + n )
      println ( '==== Groovy/Java GPars CSP Multiple elapse = ' + elapseTime )
      println ( '==== Groovy/Java GPars CSP Multiple processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
      println ( '==== Groovy/Java GPars CSP Multiple task count = ' + numberOfTasks )
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
