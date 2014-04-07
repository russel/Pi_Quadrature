#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012, 2014  Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic execute() {
  final int n = 100_000_000 // 10 times fewer than Java due to speed issues.
  final double delta = 1.0 / n
  final startTimeNanos = System.nanoTime ()
  double sum = 0.0
  for (int i = 1; i <= n; ++i) {
    sum += 1.0d / (1.0d + (( i - 0.5d) * delta) ** 2i)
  }
  final double pi = 4.0 * delta * sum
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out getClass(), pi, n, elapseTime
}

execute()
