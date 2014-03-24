#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with Java 8 Streams.
 *
 *  Copyright © 2010–2012, 2014  Russel Winder
 */

import java.util.stream.IntStream

import groovy.transform.CompileStatic

@CompileStatic
void execute(final int numberOfTasks) {
  final int n = 100000000i // 10 times fewer than Java due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ()
  final double pi = 4.0d * delta * IntStream.range(0i, n).parallel().mapToDouble{int i ->
    final double x = (i - 0.5d) * delta
    1.0d / (1.0d + x * x)
  }.sum()
  final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
  Output.out(getClass().name, pi, n, elapseTime, numberOfTasks)
}

execute 1
execute 2
execute 8
execute 32
