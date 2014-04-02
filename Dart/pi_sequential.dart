#! /usr/bin/env dart

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2014  Russel Winder
 */

import 'output.dart';

void main() {
  final n = 100000000; // 10 times fewer.
  final delta = 1.0 / n;
  final stopwatch = new Stopwatch()..start();
  var sum = 0.0;
  for (var i = 0; i < n; ++i) {
    final x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  final pi = 4.0 * delta * sum;
  final elapseTime = stopwatch.elapsedMicroseconds / 1e6;
  out('pi_sequential', pi, n, elapseTime);
}
