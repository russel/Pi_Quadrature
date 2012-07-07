#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a parallel algorithm based on using Futures.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

import java.util.concurrent.Callable
import java.util.concurrent.ScheduledThreadPoolExecutor

def execute ( final int numberOfTasks ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final executor = new ScheduledThreadPoolExecutor ( numberOfTasks )
  final futures = ( 0i ..< numberOfTasks ).collect { taskId ->
    executor.submit ( new Callable<Double> ( ) {
                        @Override public Double call ( ) {
                          final int start = 1i + taskId * sliceSize
                          final int end = ( taskId + 1i ) * sliceSize
                          double sum = 0.0d ;
                          for ( int i in start .. end ) {
                            final double x = ( i - 0.5d ) * delta
                            sum += 1.0d / ( 1.0d + x * x )
                          }
                          sum
                        }
                      } )
  }
  final double pi = 4.0d * delta * futures.sum { f -> f.get ( ) }
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  executor.shutdown ( )
  Output.out ( getClass ( ).name , pi , n , elapseTime , numberOfTasks )
}

execute 1
execute 2
execute 8
execute 32
