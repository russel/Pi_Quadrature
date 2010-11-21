#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars ParallelEnhancer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010 Russel Winder
 */

@Grab ( group = 'org.codehaus.gpars' , module = 'gpars' , version = '0.11-beta-1' )

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
  println ( "==== Groovy/Java GPars ParallelEnhancer pi = " + pi )
  println ( "==== Groovy/Java GPars ParallelEnhancer iteration count = " + n )
  println ( "==== Groovy/Java GPars ParallelEnhancer elapse = " + elapseTime )
  println ( "==== Groovy/Java GPars ParallelEnhancer processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) )
  println ( "==== Groovy/Java GPars ParallelEnhancer task count = " + numberOfTasks )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
