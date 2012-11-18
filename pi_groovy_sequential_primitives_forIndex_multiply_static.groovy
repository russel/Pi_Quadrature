#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic execute() {
  final int n = 1000000000
  final double delta = 1.0 / n
  final startTimeNanos = System.nanoTime ()
  double sum = 0.0
  for (int i = 1; i <= n; ++i) {
    final double x = (i - 0.5d) * delta
    sum += 1.0d / (1.0d + x * x)
  }
  final double pi = 4.0 * delta * sum
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out(getClass().name, pi, n, elapseTime)
}

execute()
