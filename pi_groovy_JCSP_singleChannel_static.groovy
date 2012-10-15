#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with a fork/join approach with JCSP to partition the problem
 *  and hence harness all processors available to the JVM.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

@Grab('org.codehaus.jcsp:jcsp:1.1-rc5')

import org.jcsp.lang.Channel
import org.jcsp.lang.CSProcess
import org.jcsp.lang.Parallel

void execute(int numberOfTasks) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime()
  final int sliceSize = n / numberOfTasks
  final channel = Channel.any2one()
  final processes = (0i ..< numberOfTasks).collect { taskId ->
    {-> channel.out().write(PartialSum.compute(taskId, sliceSize, delta))} as CSProcess
  }
  processes << {
      final double pi = 4.0d * delta *(0i ..< numberOfTasks).sum{(double)channel.in().read()}
      final elapseTime = (System.nanoTime() - startTimeNanos) / 1e9
      Output.out(getClass().name, pi, n, elapseTime, numberOfTasks)
    } as CSProcess
  new Parallel(processes as CSProcess[]).run()
}

execute 1
execute 2
execute 8
execute 32
