#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars ParallelEnhancer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import groovyx.gpars.ParallelEnhancer

void execute ( final int numberOfTasks ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final items = 0i ..< numberOfTasks
  ParallelEnhancer.enhanceInstance ( items )
  final pi = 4.0d * delta * items.collectParallel { taskId ->
    ( new ProcessSlice ( taskId , sliceSize , delta ) ).compute ( )
  }.sumParallel ( )
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( '==== Groovy/Java GPars ParallelEnhancer pi = ' + pi )
  println ( '==== Groovy/Java GPars ParallelEnhancer iteration count = ' + n )
  println ( '==== Groovy/Java GPars ParallelEnhancer elapse = ' + elapseTime )
  println ( '==== Groovy/Java GPars ParallelEnhancer processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
  println ( '==== Groovy/Java GPars ParallelEnhancer task count = ' + numberOfTasks )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
