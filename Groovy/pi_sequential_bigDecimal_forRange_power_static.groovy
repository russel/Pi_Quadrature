#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2014  Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic execute() {
  final n = 100_000 // 10,000 times fewer than Java due to speed issues.
  final delta = 1.0 / n
  final startTime = System.nanoTime()
  def sum = 0.0
  for (i in 1 .. n) { sum += 1 /(1 + ((i - 0.5) * delta) ** 2) }
  final pi = 4 * delta * sum
  final elapseTime = (System.nanoTime() - startTime) / 1e9
  Output.out getClass(), pi, n, elapseTime
}

execute()
