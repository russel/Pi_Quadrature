#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a thread-based algorithm, enforcing primitive types
 *  throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import java.util.concurrent.LinkedBlockingQueue

def execute ( numberOfThreads ) {
  final n = 1000000000
  final delta = 1.0 / n
  final startTime = System.nanoTime ( )
  final sliceSize = n / numberOfThreads
  final results = new LinkedBlockingQueue ( )
  ( 0 ..< numberOfThreads ).each { id -> new Thread ( { results << PartialSum.compute ( id , sliceSize , delta ) } ).start ( ) }
  def sum = 0.0
  ( 0 ..< numberOfThreads ).each { sum += results.take ( ) }
  final pi = 4.0 * delta * sum
  final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
  Output.out ( getClass ( ).name , pi , n , elapseTime , numberOfThreads )
}

execute 1
execute 2
execute 8
execute 32
