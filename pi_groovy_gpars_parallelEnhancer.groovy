#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars ParallelEnhancer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2011 Russel Winder
 */

//@Grab ( 'org.codehaus.gpars:gpars:0.12' )
@Grab ( 'org.codehaus.gpars:gpars:1.0-SNAPSHOT' )

import groovyx.gpars.ParallelEnhancer

void execute ( final int numberOfTasks ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final items = [ ]
  for ( int i in 0i ..< numberOfTasks ) { items << i }
  ParallelEnhancer.enhanceInstance ( items )
  final pi = 4.0d * delta * items.collectParallel { taskId ->
    final int start = 1i + taskId * sliceSize
    final int end = ( taskId + 1i ) * sliceSize
    double sum = 0.0d ;
    for ( int i in start .. end ) {
      final double x = ( i - 0.5d ) * delta
      sum += 1.0d / ( 1.0d + x * x )
    }
    sum
  }.sumParallel ( )
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( '==== Groovy GPars ParallelEnhancer pi = ' + pi )
  println ( '==== Groovy GPars ParallelEnhancer iteration count = ' + n )
  println ( '==== Groovy GPars ParallelEnhancer elapse = ' + elapseTime )
  println ( '==== Groovy GPars ParallelEnhancer processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
  println ( '==== Groovy GPars ParallelEnhancer task count = ' + numberOfTasks )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
