#! /usr/bin/env node

/*
 *  Calculation of π using quadrature realized with a parallel algorithm in cluster.
 *
 *  This expects nodejs 4.
 *
 *  Copyright © 2015 Fredrik Liljegren
 *
 *  Interresting note: I started using `let` instead of `var` everywhere, but when using `let` in
 *  the partialSum function, the calculation took very much longer to perform.  I guess the scoping
 *  of the variable adds some extra overhead…
 */

'use strict';

const cluster = require('cluster')
const out     = require('./output.js')
const Promise = require('bluebird')
var async     = require('asyncawait/async')
var await     = require('asyncawait/await')

const partialSum = (id, sliceSize, ð) => {
  const start = 1 + id * sliceSize
  const end   = (id + 1) * sliceSize + 1
  var   Σ     = 0

  for (var i = start; i < end; i++) {
    var x = (i - 0.5) * ð
    Σ += 1 / (1 + x * x)
  }
  return Σ
}

const execute = async((tasks) => {
  const n         = 1000000000
  const ð         = 1.0 / n
  const startTime = Date.now()
  const sliceSize = n / tasks

  const promises = []
  for (var i = 0; i < tasks; i++) {
    const worker = cluster.fork({id: i, sliceSize: sliceSize, ð: ð})

    promises.push(new Promise((resolve, reject) => {
      worker.on('message', (event) => {resolve(event)})
    }))
  }

  // Wait for all workers
  const Σ = await(Promise.all(promises).then((ðs) => ðs.reduce((Σ, ð) => Σ + ð, 0)))
  const π = 4.0 * ð * Σ
  const elapseTime = (Date.now() - startTime) / 1e3

  out('pi_parallel_cluster', π, n, elapseTime, tasks)
})

if (cluster.isMaster) {
  async(() => {
    await(execute(1))
    await(execute(2))
    await(execute(8))
    await(execute(32))
  })().done()
}
else {
  // This is a worker process, get slice from env and perform calculation.
  const ð         = parseFloat(process.env.ð)
  const id        = parseInt(process.env.id)
  const sliceSize = parseFloat(process.env.sliceSize)

  cluster.worker.send(partialSum(id, sliceSize, ð))
  cluster.worker.kill()
}
