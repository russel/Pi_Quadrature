#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a thread-based algorithm, enforcing primitive types
 *  throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import java.util.concurrent.LinkedBlockingQueue

def execute ( numberOfThreads ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTime = System.nanoTime ( )
  final int sliceSize = n / numberOfThreads
  final results = new LinkedBlockingQueue ( )
  final partialSum = { id -> results << PartialSum.compute ( id , sliceSize , delta ) }
  ( 0 ..< numberOfThreads ).each { id -> new Thread ( partialSum.curry ( id ) ).start ( ) }
  double sum = 0.0d
  ( 0 ..< numberOfThreads ).each { sum += results.take ( ) }
  final double pi = 4.0d * delta * sum
  final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
  Output.out ( getClass ( ).name , pi , n , elapseTime , numberOfThreads )
}

execute 1
execute 2
execute 8
execute 32
