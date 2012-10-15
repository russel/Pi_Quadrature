#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final int n = 10000000i // 100 times fewer than Java due to speed issues.
final double delta = 1.0d / n
final startTime = System.nanoTime()
final double pi = 4.0d * delta * (1i .. n).sum {i ->
  final double x = (i - 0.5d) * delta
  1.0d / (1.0d + x * x)
}
final elapseTime = (System.nanoTime() - startTime) / 1e9
Output.out(getClass().name, pi, n, elapseTime)
