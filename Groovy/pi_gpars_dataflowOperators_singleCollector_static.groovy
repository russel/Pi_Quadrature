#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with GPars dataflow operators and channels, using a single
 *  collector channel to avoid the accumulator operator.
 *
 *  Copyright © 2014  Russel Winder
 */

import groovyx.gpars.dataflow.DataflowQueue
import static groovyx.gpars.dataflow.Dataflow.operator

import groovy.transform.CompileStatic

@CompileStatic
void execute(final int operatorCount) {
  final n = 1_000_000_000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ()
  final sliceSize = (int)(n / operatorCount)
  final partialSums = new DataflowQueue()
  final scatterQueues = (0i ..< operatorCount).collect{new DataflowQueue()}
  (0 ..< operatorCount).each {index ->
    operator(inputs: [scatterQueues[index]], outputs: [partialSums]) {int i ->
      bindOutput 0, PartialSum.staticCompile(i, sliceSize, delta)
    }
    scatterQueues[index] << index
  }
  final pi = 4.0d * delta * (double)((0 ..< operatorCount).sum{partialSums.val})
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime, operatorCount
}

execute 1
execute 2
execute 8
execute 32
