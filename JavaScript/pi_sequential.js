#! /usr/bin/env jjs

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2014  Russel Winder
 */

'use strict';

const out = require('./output.js')

var n = 1000000000
var ð = 1.0 / n
var startTime = Date.now()

var Σ = 0.0

for (var i = 0; i < n; ++i) {
  var x = (i - 0.5) * ð
  Σ += 1.0 / (1.0 + x * x)
}
var π = 4.0 * ð * Σ
var elapseTime = (Date.now() - startTime) / 1e3
out('pi_sequential', π, n, elapseTime, 1)
