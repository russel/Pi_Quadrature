#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a thread-based algorithm, enforcing primitive types
 *  throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import java.util.concurrent.LinkedBlockingQueue

def partialSum ( int id , int sliceSize , double delta ) { 
  final int start = 1i + id * sliceSize
  final int end = ( id + 1i ) * sliceSize
  double sum = 0.0d
  for ( int i in start .. end ) {
    final double x = ( i - 0.5d ) * delta
    sum += 1.0d / ( 1.0d + x * x )
  }
  sum
}

def execute ( numberOfThreads ) {
  final n = 100000000 // 10 times fewer than Java due to speed issues.
  final delta = 1.0 / n
  final startTime = System.nanoTime ( )
  final int sliceSize = n / numberOfThreads
  final results = new LinkedBlockingQueue ( )
  ( 0 ..< numberOfThreads ).each { id -> new Thread ( { results << partialSum ( id , sliceSize , delta ) } ).start ( ) }
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
