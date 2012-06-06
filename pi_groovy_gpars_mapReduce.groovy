#! /usr/bin/env groovy

import static groovyx.gpars.GParsPool.withPool

/*
 *  Calculation of Pi using quadrature realized with a map/reduce approach with GPars and hence harness all
 *  processors available to the JVM.
 *
 *  Copyright © 2012 Russel Winder
 */
void execute ( final int numberOfTasks ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final pi
  withPool {
    pi = 4.0d * delta * ( 0i ..< numberOfTasks ).parallel.map { taskId ->
      final int start = 1i + taskId * sliceSize
      final int end = ( taskId + 1i ) * sliceSize
      double sum = 0.0d
      for ( int i in start .. end ) {
        final double x = ( i - 0.5d ) * delta
        sum += 1.0d / ( 1.0d + x * x )
      }
      sum
    }.sum ( )
  }
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( '==== Groovy GPars MapReduce pi = ' + pi )
  println ( '==== Groovy GPars MapReduce iteration count = ' + n )
  println ( '==== Groovy GPars MapReduce elapse = ' + elapseTime )
  println ( '==== Groovy GPars MapReduce processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
  println ( '==== Groovy GPars MapReduce task count = ' + numberOfTasks )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
