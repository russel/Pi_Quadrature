#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012, 2014  Russel Winder
 */

final int n = 1_000_000_000
final double delta = 1.0 / n
final startTimeNanos = System.nanoTime()
final double pi = 4.0 * delta * PartialSum.dynamicCompile(0, n, delta)
final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
Output.out getClass(), pi, n, elapseTime
