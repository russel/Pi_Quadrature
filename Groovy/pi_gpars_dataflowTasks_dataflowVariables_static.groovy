#! /usr/bin/env groovy

/*
 *  Caluclation of π using quadrature realized with GPars dataflow tasks and variables.
 *
 *  Copyright © 2011–2012, 2014  Russel Winder
 */

import groovyx.gpars.dataflow.DataflowVariable
import static groovyx.gpars.dataflow.Dataflow.task

void execute(final operatorCount) {
  final n = 1_000_000_000
  final delta = 1.0 / n
  final startTimeNanos = System.nanoTime()
  final sliceSize = (int)(n / operatorCount)
  final partialSums = []
  (0 ..< operatorCount).each {index ->
    final variable = new DataflowVariable()
    partialSums << variable
    task {variable << PartialSum.staticCompile(index, sliceSize, delta)}
  }
  final pi = 4.0 * delta * partialSums.sum{it.val}
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime, operatorCount
}

execute 1
execute 2
execute 8
execute 32
