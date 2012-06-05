#! /usr/bin/env groovy

/*
 *  Caluclation of Pi using quadrature realized with GPars dataflow variables.
 *
 *  Copyright © 2011–2012 Russel Winder
 */

import groovyx.gpars.dataflow.DataflowQueue
import static groovyx.gpars.dataflow.Dataflow.task

void execute ( final int operatorCount ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / operatorCount
  final partialSums = new DataflowQueue ( )
  ( 0 ..< operatorCount ).each { index ->
    task {
      final int start = 1i + index * sliceSize
      final int end = ( index + 1i ) * sliceSize
      double sum = 0.0d
      for ( int i in start .. end ) {
        final double x = ( i - 0.5d ) * delta
        sum += 1.0d / ( 1.0d + x * x )
      }
      partialSums << sum
    }
  }
  final double pi = 4.0d * delta * ( 0 ..< operatorCount ).sum { partialSums.val }
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( '==== Groovy GPars DataflowQueue pi = ' + pi )
  println ( '==== Groovy GPars DataflowQueue iteration count = ' + n )
  println ( '==== Groovy GPars DataflowQueue elapse = ' + elapseTime )
  println ( '==== Groovy GPars DataflowQueue processor count = ' + Runtime.runtime.availableProcessors ( ) ) ;
  println ( '==== Groovy GPars DataflowQueue operator count = ' + operatorCount )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
