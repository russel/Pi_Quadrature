#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a fork/join approach with GPars Parallelizer to
 *  partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import groovyx.gpars.GParsPool

void execute(final numberOfTasks) {
    GParsPool.withPool {
        final n = 1000000000
        final delta = 1.0 / n
        final startTimeNanos = System.nanoTime()
        final sliceSize = (int)(n / numberOfTasks)
        final pi = 4.0 * delta * (0 ..< numberOfTasks).collectParallel {taskId ->
            new ProcessSlice(taskId, sliceSize, delta).compute()
        }.sumParallel()
        final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
        Output.out(getClass().name, pi, n, elapseTime, numberOfTasks)
    }
}

execute 1
execute 2
execute 8
execute 32
