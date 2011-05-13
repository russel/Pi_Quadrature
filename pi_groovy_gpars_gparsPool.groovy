#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars Parallelizer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010--2011 Russel Winder
 */

@Grab ( 'org.codehaus.gpars:gpars:0.12-beta-1' )

import groovyx.gpars.GParsPool

void execute ( final int numberOfTasks ) {
  GParsPool.withPool {
    final int n = 100000000i // 10 times fewer due to speed issues.
    final double delta = 1.0d / n
    final startTimeNanos = System.nanoTime ( )
    final int sliceSize = n / numberOfTasks
    final items = [ ] ; for ( int i in 0i ..< numberOfTasks ) { items << i }
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
    println ( '==== Groovy GPars GParsPool pi = ' + pi )
    println ( '==== Groovy GPars GParsPool iteration count = ' + n )
    println ( '==== Groovy GPars GParsPool elapse = ' + elapseTime )
    println ( '==== Groovy GPars GParsPool processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
    println ( '==== Groovy GPars GParsPool task count = ' + numberOfTasks )
  }
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
