#! /usr/bin/env jjs

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2014  Russel Winder
 */

load('output.js')

var n = 100000000 // 10 times fewer.
var delta = 1.0 / n
var startTime = Date.now()
var sum = 0.0
for (var i = 0; i < n; ++i) {
  var x = (i - 0.5) * delta
  sum += 1.0 / (1.0 + x * x)
}
var pi = 4.0 * delta * sum
var elapseTime = (Date.now() - startTime) / 1e3
out('pi_sequential', pi, n, elapseTime)
