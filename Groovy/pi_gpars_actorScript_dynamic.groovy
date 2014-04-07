#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with GPars actors. Scripty.
 *
 *  Copyright © 2009–2012, 2014  Russel Winder.
 */

import groovyx.gpars.actor.Actor
import groovyx.gpars.group.DefaultPGroup

void execute(final actorCount) {
  final n = 1_000_000_000
  final delta = 1.0 / n
  final startTimeNanos = System.nanoTime ()
  final sliceSize = (int)(n / actorCount)
  final group = new DefaultPGroup(actorCount + 1)
  final accumulator = group.messageHandler {
    def sum = 0.0
    def count = 0
    when {result ->
      sum += result
      if (++count == actorCount) {
        final pi = 4.0 * delta * sum
        final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
        Output.out getClass(), pi, n, elapseTime, actorCount
        terminate()
      }
    }
  }
  (0 ..< actorCount).each {index ->
    group.actor {accumulator << PartialSum.dynamicCompile(index, sliceSize, delta)}
  }
  accumulator.join()
}

execute 1
execute 2
execute 8
execute 32
