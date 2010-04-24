#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars CSP to partition the
 *  problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010 Russel Winder
 */

@Grab ( group = 'org.codehaus.jcsp' , module = 'jcsp' , version = '1.1-rc5-SNAPSHOT' )
@Grab ( group = 'org.codehaus.gpars' , module = 'gpars' , version = '0.10-beta-1-SNAPSHOT' )

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess

import groovyx.gpars.csp.PAR

void execute ( final int numberOfTasks ) {
  final long n = 1000000000l
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final long sliceSize = n / numberOfTasks
  final channels = Channel.one2oneArray ( numberOfTasks )
  final processes = [ ]
  for ( i in 0 ..< numberOfTasks ) { processes << new ProcessSlice_JCSP ( i , sliceSize , delta , channels[i].out ( ) ) }
  processes << new CSProcess ( ) {
    public void run ( ) {
      double sum = 0.0d
      for ( c in channels ) { sum += (double) c.in ( ).read ( ) }
      final double pi = 4.0d * sum * delta
      final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      System.out.println ( "==== Groovy/Java GPars CSP Multiple pi = " + pi )
      System.out.println ( "==== Groovy/Java GPars CSP Multiple iteration count = " + n )
      System.out.println ( "==== Groovy/Java GPars CSP Multiple elapse = " + elapseTime )
      System.out.println ( "==== Groovy/Java GPars CSP Multiple processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) )
      System.out.println ( "==== Groovy/Java GPars CSP Multiple task count = " + numberOfTasks )
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
