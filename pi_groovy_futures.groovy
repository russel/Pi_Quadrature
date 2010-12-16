#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a parallel algorithm based on using Futures.
 *
 *  Copyright © 2009-10 Russel Winder
 */

import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.ExecutionException
import java.util.concurrent.Future
import java.util.concurrent.ScheduledThreadPoolExecutor

// This is Groovy and so extraordinarily slow compared to Java.  Use primitive types though so as to avoid
// really bad performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as
// well as the variables.

def execute ( final int numberOfTasks ) {
  final long n = 100000000l // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final long startTimeNanos = System.nanoTime ( )
  final long sliceSize = n / numberOfTasks
  final ExecutorService executor = new ScheduledThreadPoolExecutor ( numberOfTasks )
  final Future<Double>[] futures = new Future<Double> [ numberOfTasks ]
  for ( int i = 0l ; i < numberOfTasks ; ++i ) {
    final taskId = i
    futures[i] = executor.submit ( new Callable<Double> ( ) {
                                     @Override public Double call ( ) {
                                       final long start = 1l + taskId * sliceSize
                                       final long end = ( taskId + 1l ) * sliceSize
                                       double sum = 0.0d ;
                                       for ( long j = start ; j <= end ; ++j ) {
                                         final double x = ( j - 0.5d ) * delta
                                         sum += 1.0d / ( 1.0d + x * x )
                                       }
                                       return sum
                                     }
                                   } )
  }
  double sum = 0.0d
  for ( Future<Double> f : futures ) {
    try { sum += f.get ( ) }
    catch ( final InterruptedException ie ) { throw new RuntimeException ( ie ) } 
    catch ( final ExecutionException ee ) { throw new RuntimeException ( ee ) } 
  }
  final double pi = 4.0d * sum * delta
  final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
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
