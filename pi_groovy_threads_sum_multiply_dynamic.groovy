#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a thread-based algorithm, enforcing primitive types
 *  throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import java.util.concurrent.LinkedBlockingQueue

def execute ( numberOfThreads ) {
  final int n = 100000000i // 10 times fewer than Java due to speed issues.
  final double delta = 1.0d / n
  final startTime = System.nanoTime ( )
  final int sliceSize = n / numberOfThreads
  final results = new LinkedBlockingQueue ( )
  final partialSum = { id ->
    final int start = 1i + id * sliceSize
    final int end = ( id + 1i ) * sliceSize
    double sum = 0.0d
    for ( int i in start .. end ) {
      final double x = ( i - 0.5d ) * delta
      sum += 1.0d / ( 1.0d + x * x )
    }
    results.put ( sum )
  }
  ( 0 ..< numberOfThreads ).each { i -> new Thread ( partialSum.curry ( i ) ).start ( ) }
  double sum = 0.0d
  ( 0 ..< numberOfThreads ).each { sum += results.take ( ) }
  final double pi = 4.0d * delta * sum
  final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
  Output.out ( getClass ( ).name , pi , n , elapseTime )
}

execute 1
execute 2
execute 8
execute 32
