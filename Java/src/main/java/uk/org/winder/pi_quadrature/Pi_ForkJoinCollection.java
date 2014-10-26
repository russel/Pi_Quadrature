/*
 *  Calculation of π using quadrature realized with a fork/join approach.  This uses a collection to store
 *  the futures.
 *
 *  Copyright © 2009–2014  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.ArrayList;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.Future;

public class Pi_ForkJoinCollection {

  private static void execute(final int numberOfTasks) {
    final int n = 1_000_000_000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final ArrayList<Callable<Double>> callables = new ArrayList<>();
    for (int i = 0; i < numberOfTasks; ++i) {
      final int taskId = i;
      callables.add(() -> {
          final int start = 1 + taskId * sliceSize;
          final int end = (taskId + 1) * sliceSize;
          double sum = 0.0;
          for (int j = start; j <= end; ++j) {
            final double x = (j - 0.5) * delta;
            sum += 1.0 / (1.0 + x * x);
          }
          return sum;
        });
    }
    final ForkJoinPool pool = new ForkJoinPool(numberOfTasks);
    double sum = 0.0;
    for (final Future<Double> f: pool.invokeAll(callables)) {
      try { sum += f.get(); }
      catch(InterruptedException | ExecutionException e) { throw new RuntimeException(e); }
    }
    //pool.shutdown();
    final double pi = 4.0 * delta * sum;
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out(Pi_ForkJoinCollection.class, pi, n, elapseTime, numberOfTasks);
  }

  public static void main(final String[] args) {
    Pi_ForkJoinCollection.execute(1);
    Pi_ForkJoinCollection.execute(2);
    Pi_ForkJoinCollection.execute(8);
    Pi_ForkJoinCollection.execute(32);
  }
}
