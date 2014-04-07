#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a fork/join approach with GPars ParallelEnhancer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012, 2014  Russel Winder
 */

import groovyx.gpars.ParallelEnhancer

void execute(final numberOfTasks) {
  final n = 1_000_000_000
  final delta = 1.0 / n
  final startTimeNanos = System.nanoTime ()
  final sliceSize = (int)(n / numberOfTasks)
  final items = 0 ..< numberOfTasks
  ParallelEnhancer.enhanceInstance(items)
  final pi = 4.0 * delta * items.collectParallel{taskId ->
    PartialSum.staticCompile(taskId, sliceSize, delta)
  }.sumParallel()
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime, numberOfTasks
}

execute 1
execute 2
execute 8
execute 32
