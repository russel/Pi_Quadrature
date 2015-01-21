/*
 *  Calculation of π using quadrature realized with a parallel algorithm using
 *  callables, futures and executors.
 *
 *  Copyright © 2012–2014  Russel Winder
 */

import java.util.concurrent { CompletableFuture }
import java.util.\ifunction { Supplier }

import uk.org.winder.pi_quadrature.tools { outputN }

void execute(Integer numberOfTasks) {
  value n = 100_000_000; // 10 times fewer than Java due to speed issues.
  value delta = 1.0 / n;
  value startTime = system.nanoseconds;
  Integer sliceSize = n / numberOfTasks;
  class Task(Integer id) satisfies Supplier<Float> {
    shared actual Float get() {
      value start = 1 + id * sliceSize;
      value end = (id + 1) * sliceSize;
      return (start..end).map((Integer i) {
        value x = (i - 0.5) * delta;
        return 1.0 / (1.0 + x * x);
      }).reduce(plus<Float>);
    }
  }
  value pi = 4.0 * delta * sum({for (f in [for (i in 1..numberOfTasks) CompletableFuture<Float>.supplyAsync(Task(i))]) f.get()});
  value elapseTime = (system.nanoseconds - startTime) / 1.0e9;
  outputN("pi_futures_map_reduce", pi, n, elapseTime, numberOfTasks);
}

"Calculate π using quadrature realized with a parallel algorithm using callables, futures and executors."
by("Russel Winder")
shared void run() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
}