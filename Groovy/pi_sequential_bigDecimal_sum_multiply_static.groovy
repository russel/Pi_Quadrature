#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2013  Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic execute() {
  final n = 100000 // 10000 times fewer than Java due to speed issues.
  final delta = 1.0 / n
  final startTime = System.nanoTime()
  final pi = 4 * delta * (Double)((1 .. n).sum{Integer i ->
    final x = (i - 0.5) * delta
    1 / (1 + x * x)
  })
  final elapseTime = (System.nanoTime() - startTime) / 1e9
  Output.out(getClass().name, pi, n, elapseTime)
}

execute()
