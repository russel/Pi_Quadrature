#! /usr/bin/env groovy

import groovyx.gpars.GParsPool

/*
 *  Calculation of Pi using quadrature realized with a map/reduce approach with GPars and hence harness all
 *  processors available to the JVM.
 *
 *  Copyright © 2012 Russel Winder
 */
void execute ( final int numberOfTasks ) {
  GParsPool.withPool {
    final int n = 1000000000i
    final double delta = 1.0d / n
    final startTimeNanos = System.nanoTime ( )
    final int sliceSize = n / numberOfTasks
    final double pi = 4.0d * delta * ( 0i ..< numberOfTasks ).parallel.map { taskId ->
      PartialSum.compute ( taskId , sliceSize , delta )
    }.sum ( )
    final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
    Output.out ( 'Groovy GPars MapReduce Static' , pi , n , elapseTime , numberOfTasks )
  }
}

execute ( 1 )
execute ( 2 )
execute ( 8 )
execute ( 32 )
