#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with GPars Parallelizer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import groovyx.gpars.GParsPool

void execute ( final int numberOfTasks ) {
  GParsPool.withPool {
    final int n = 1000000000i
    final double delta = 1.0d / n
    final startTimeNanos = System.nanoTime ( )
    final int sliceSize = n / numberOfTasks
    final pi = 4.0d * delta * ( 0i ..< numberOfTasks ).collectParallel { taskId ->
      ( new ProcessSlice ( taskId , sliceSize , delta ) ).compute ( )
    }.sumParallel ( )
    final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
    println ( '==== Groovy/Java GPars GParsPool pi = ' + pi )
    println ( '==== Groovy/Java GPars GParsPool iteration count = ' + n )
    println ( '==== Groovy/Java GPars GParsPool elapse = ' + elapseTime )
    println ( '==== Groovy/Java GPars GParsPool processor count = ' + Runtime.getRuntime ( ).availableProcessors ( ) )
    println ( '==== Groovy/Java GPars GParsPool task count = ' + numberOfTasks )
  }
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
