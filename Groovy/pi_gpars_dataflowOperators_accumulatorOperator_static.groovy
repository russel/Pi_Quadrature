#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with GPars dataflow operators and channels, using an
 *  accumulator operator.
 *
 *  Copyright © 2014  Russel Winder
 */

import groovyx.gpars.dataflow.DataflowVariable
import static groovyx.gpars.dataflow.Dataflow.operator

import groovy.transform.CompileStatic

//@CompileStatic
void execute(final int operatorCount) {
  final n = 1_000_000_000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ()
  final sliceSize = (int)(n / operatorCount)
  final scatterChannels = (0i ..< operatorCount).collect{new DataflowVariable<Integer>()}
  final gatherChannels = (0i ..< operatorCount).collect{new DataflowVariable<Double>()}
  final sum_v = new DataflowVariable<Double>()
  operator(inputs: gatherChannels, outputs: [sum_v], stateObject: [accumulator: 0.0d, count: 0i]){Double... values ->
    while (stateObject.count < operatorCount) { stateObject.accumulator += values.sum() }
    bindOutput 0, (double)(stateObject.accumulator)
  }
  (0 ..< operatorCount).each{index ->
    operator(inputs: [scatterChannels[index]], outputs: [gatherChannels[index]]){int i ->
      bindOutput 0, PartialSum.dynamicCompile(i, sliceSize, delta)
    }
    scatterChannels[index] << index
  }
  final pi = 4.0d * delta * sum_v.val
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime, operatorCount
}

execute 1
execute 2
execute 8
execute 32
