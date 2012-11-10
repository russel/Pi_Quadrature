#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final n = 1000000 // 1000 times fewer than Java due to speed issues.
final delta = 1.0 / n
final startTimeNanos = System.nanoTime()
def sum = 0.0
for (i = 1; i <= n; ++i) { sum += 1.0 / (1.0 + (( i - 0.5) * delta) ** 2) }
final pi = 4.0 * delta * sum
final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
Output.out(getClass().name,  pi, n, elapseTime)
