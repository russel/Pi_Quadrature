#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a fork/join approach with GPars Parallelizer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012, 2014  Russel Winder
 */

import groovyx.gpars.GParsPool

void execute(final numberOfTasks) {
  GParsPool.withPool {
    final n = 10_000_000 // 100 times fewer than Java due to speed issues.
    final delta = 1.0 / n
    final startTimeNanos = System.nanoTime ()
    final sliceSize = (int)(n / numberOfTasks)
    final pi = 4.0 * delta * (0 ..< numberOfTasks).collectParallel {taskId ->
      PartialSum.dynamicCompile(taskId, sliceSize, delta)
    }.sumParallel()
    final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
    Output.out getClass(), pi, n, elapseTime, numberOfTasks
  }
}

execute 1
execute 2
execute 8
execute 32
