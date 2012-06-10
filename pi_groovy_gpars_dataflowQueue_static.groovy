#! /usr/bin/env groovy

/*
 *  Caluclation of Pi using quadrature realized with GPars dataflow variables.
 *
 *  Copyright © 2011–2012 Russel Winder
 */

import groovyx.gpars.dataflow.DataflowQueue
import static groovyx.gpars.dataflow.Dataflow.task

void execute ( final int operatorCount ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / operatorCount
  final partialSums = new DataflowQueue ( )
  ( 0 ..< operatorCount ).each { index ->
    task { partialSums << PartialSum.compute ( index , sliceSize , delta ) }
  }
  final double pi = 4.0d * delta * ( 0 ..< operatorCount ).sum { partialSums.val }
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  Output.out ( 'Groovy GPars DataflowQueue Static' , pi , n , elapseTime , operatorCount )
}

execute ( 1 )
execute ( 2 )
execute ( 8 )
execute ( 32 )
