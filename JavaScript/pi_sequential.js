#! /usr/bin/env jjs_es6

/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Requires ES6. e,g. jjs --language=es6 pi_sequential.js
 *
 *  Copyright © 2014, 2015  Russel Winder
 */

'use strict'

load('output.js')

const n = 100000000 // 10 times fewer.
const δ = 1.0 / n
const startTime = Date.now()
let Σ = 0.0
for (let i = 0; i < n; ++i) {
  const x = (i - 0.5) * δ
  Σ += 1.0 / (1.0 + x * x)
}
const π = 4.0 * δ * Σ
const elapseTime = (Date.now() - startTime) / 1e3
out('Sequential', π, n, elapseTime)
