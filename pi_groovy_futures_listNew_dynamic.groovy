#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a parallel algorithm based on using Futures.
 *
 *  Copyright © 2009–2012 Russel Winder
 */

import java.util.concurrent.Callable
import java.util.concurrent.ScheduledThreadPoolExecutor

def execute(final numberOfTasks) {
  final n = 1000000000
  final delta = 1.0 / n
  final startTimeNanos = System.nanoTime ()
  final sliceSize = (int)(n / numberOfTasks)
  final executor = new ScheduledThreadPoolExecutor(numberOfTasks)
  final futures = (0 ..< numberOfTasks).collect {taskId ->
    executor.submit(new Callable<Double>() {
                        @Override public Double call() { PartialSum.dynamicCompile(taskId, sliceSize, delta) }
                      })
  }
  final pi = 4.0 * delta * futures.sum {f -> f.get()}
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  executor.shutdown()
  Output.out(getClass().name, pi, n, elapseTime, numberOfTasks)
}

execute 1
execute 2
execute 8
execute 32
