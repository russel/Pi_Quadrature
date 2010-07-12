#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars ParallelEnhancer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010 Russel Winder
 */

@Grab ( group = 'org.codehaus.gpars' , module = 'gpars' , version = '0.10' )

import groovyx.gpars.ParallelEnhancer

void execute ( final int numberOfTasks ) {
  final long n = 1000000000l
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final long sliceSize = n / numberOfTasks
  final items = [ ] ; for ( i in 0 ..< numberOfTasks ) { items << i }
  ParallelEnhancer.enhanceInstance ( items )
  final pi = 4.0d * delta * items.collectParallel { taskId ->
    ( new ProcessSlice ( taskId , sliceSize , delta ) ).compute ( )
  }.sumParallel ( )
  final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  System.out.println ( "==== Groovy/Java GPars ParallelEnhancer pi = " + pi )
  System.out.println ( "==== Groovy/Java GPars ParallelEnhancer iteration count = " + n )
  System.out.println ( "==== Groovy/Java GPars ParallelEnhancer elapse = " + elapseTime )
  System.out.println ( "==== Groovy/Java GPars ParallelEnhancer processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) )
  System.out.println ( "==== Groovy/Java GPars ParallelEnhancer task count = " + numberOfTasks )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
