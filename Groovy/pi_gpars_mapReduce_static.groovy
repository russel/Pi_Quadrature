#! /usr/bin/env groovy

import groovyx.gpars.GParsPool

/*
 *  Calculation of π using quadrature realized with a map/reduce approach with GPars and hence harness all
 *  processors available to the JVM.
 *
 *  Copyright © 2012 Russel Winder
 */
void execute(final numberOfTasks) {
  GParsPool.withPool {
    final n = 1000000000
    final delta = 1.0 / n
    final startTimeNanos = System.nanoTime ()
    final sliceSize = (int)(n / numberOfTasks)
    final pi = 4.0 * delta * (0 ..< numberOfTasks).parallel.map {taskId ->
      PartialSum.staticCompile(taskId, sliceSize, delta)
    }.sum()
    final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
    Output.out(getClass().name, pi, n, elapseTime, numberOfTasks)
  }
}

execute 1
execute 2
execute 8
execute 32
