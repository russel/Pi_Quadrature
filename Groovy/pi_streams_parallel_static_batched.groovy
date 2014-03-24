#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with Java 8 Streams.
 *
 *  Copyright © 2010–2012, 2014  Russel Winder
 */

import java.util.stream.IntStream

import groovy.transform.CompileStatic

@CompileStatic
void execute(final int numberOfTasks) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ()
  final int sliceSize = (int)(n / numberOfTasks) // Division create a BigDecimal!
  final double pi = 4.0d * delta * IntStream.range(0i, numberOfTasks).parallel().mapToDouble{int taskId ->
    PartialSum.staticCompile(taskId, sliceSize, delta)
   }.sum()
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9d
  Output.out(getClass().name, pi, n, elapseTime, numberOfTasks)
}

execute 1
execute 2
execute 8
execute 32
