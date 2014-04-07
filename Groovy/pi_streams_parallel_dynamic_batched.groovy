#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with Java 8 Streams.
 *
 *  Copyright © 2010–2012, 2014  Russel Winder
 */

import java.util.stream.IntStream

void execute(final int numberOfTasks) {
  final int n = 1_000_000_000
  final double delta = 1.0 / n
  final startTimeNanos = System.nanoTime ()
  final int sliceSize = n / numberOfTasks
  final double pi = 4.0 * delta * IntStream.range(0i, numberOfTasks).parallel().mapToDouble{int taskId ->
    PartialSum.dynamicCompile(taskId, sliceSize, delta)
  }.sum()
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime, numberOfTasks
}

execute 1
execute 2
execute 8
execute 32
