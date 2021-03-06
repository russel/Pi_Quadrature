#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with GPars dataflow operators and channels, using an
 *  accumulator operator.
 *
 *  Copyright © 2014  Russel Winder
 */

import groovyx.gpars.dataflow.DataflowVariable
import static groovyx.gpars.dataflow.Dataflow.operator
import static groovyx.gpars.dataflow.Dataflow.select
import static groovyx.gpars.dataflow.Dataflow.task

import groovy.transform.CompileStatic

@CompileStatic
void execute(final int operatorCount) {
  final n = 1_000_000_000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ()
  final sliceSize = (int)(n / operatorCount)
  final scatterChannels = (0i ..< operatorCount).collect{new DataflowVariable<Integer>()}
  final gatherChannels = (0i ..< operatorCount).collect{new DataflowVariable<Double>()}
  final accumulator = task{
    final selector = select gatherChannels
    return  (0i ..< operatorCount).inject(0.0d){t, i -> t + (double)(selector().value)}
  }
  (0 ..< operatorCount).each{index ->
    operator(inputs: [scatterChannels[index]], outputs: [gatherChannels[index]]){int i ->
      bindOutput 0, PartialSum.staticCompile(i, sliceSize, delta)
    }
    scatterChannels[index] << index
  }
  final pi = 4.0d * delta * (double)(accumulator.get())
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime, operatorCount
}

execute 1
execute 2
execute 8
execute 32
