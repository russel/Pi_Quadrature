#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final int n = 1000000000i
final double delta = 1.0d / n
final startTimeNanos = System.nanoTime()
final double pi = 4.0d * delta * PartialSum.staticCompile(0, n, delta)
final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
Output.out(getClass().name, pi, n, elapseTime)
