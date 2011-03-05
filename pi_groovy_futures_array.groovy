#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a parallel algorithm based on using Futures.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.ScheduledThreadPoolExecutor

// This is Groovy and so extraordinarily slow compared to Java.  Use primitive types though so as to avoid
// really bad performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as
// well as the variables.

def execute ( final int numberOfTasks ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / numberOfTasks
  final ExecutorService executor = new ScheduledThreadPoolExecutor ( numberOfTasks )
  final Future<Double>[] futures = new Future<Double> [ numberOfTasks ]
  for ( int i in 0i ..< numberOfTasks ) {
    final taskId = i
    futures[i] = executor.submit ( new Callable<Double> ( ) {
                                     @Override public Double call ( ) {
                                       final int start = 1i + taskId * sliceSize
                                       final int end = ( taskId + 1i ) * sliceSize
                                       double sum = 0.0d ;
                                       for ( int j in start .. end ) {
                                         final double x = ( j - 0.5d ) * delta
                                         sum += 1.0d / ( 1.0d + x * x )
                                       }
                                       sum
                                     }
                                   } )
  }
  final double sum = futures.inject ( 0.0d ) { l , r -> l + r.get ( ) } 
  final double pi = 4.0d * sum * delta
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  executor.shutdown ( )
  println ( "==== Groovy Futures pi = " + pi )
  println ( "==== Groovy Futures iteration count = " + n )
  println ( "==== Groovy Futures elapse = " + elapseTime )
  println ( "==== Groovy Futures processor count = " + Runtime.runtime.availableProcessors ( ) ) ;
  println ( "==== Groovy Futures number of tasks = " + numberOfTasks )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
