/*
 *  Calculation of π using quadrature realized with a fork/join approach.  This uses a primitive array to
 *  store the futures.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

public class Pi_ForkJoinBasic_Java8 {

  private static void execute(final int numberOfTasks) {
    final int n = 1000000000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final ForkJoinPool pool = new ForkJoinPool(numberOfTasks);
    @SuppressWarnings("unchecked") final ForkJoinTask<Double>[] futures = new ForkJoinTask[numberOfTasks];
    for (int i = 0; i < numberOfTasks; ++i) {
      final int taskId = i;
      futures[taskId] = pool.submit(() -> {
            final int start = 1 + taskId * sliceSize;
            final int end = (taskId + 1) * sliceSize;
            double sum = 0.0;
            for (int ii = start; ii <= end; ++ii) {
              final double x = (ii - 0.5) * delta;
              sum += 1.0 / (1.0 + x * x);
            }
            return sum;
          });
    }
    double sum = 0.0;
    for (final ForkJoinTask<Double> f : futures) {
      try { sum += f.get(); }
      catch (InterruptedException | ExecutionException e) { throw new RuntimeException(e); }
    }
    //pool.shutdown();
    final double pi = 4.0 * delta * sum;
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Pi_ForkJoinBasic_Java8", pi, n, elapseTime, numberOfTasks);
  }

  public static void main(final String[] args) {
    Pi_ForkJoinBasic_Java8.execute(1);
    Pi_ForkJoinBasic_Java8.execute(2);
    Pi_ForkJoinBasic_Java8.execute(8);
    Pi_ForkJoinBasic_Java8.execute(32);
  }
}
