/*
 *  Calculation of π using quadrature realized with an approached based on Java 8 streams.
 *
 *  Copyright © 2013, 2014  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.stream.IntStream;

public class Pi_ParallelStream_Batched {
  private static void execute(final int numberOfTasks) {
    final int n = 1_000_000_000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final double pi = 4.0 * delta * IntStream.range(0, numberOfTasks).parallel().mapToDouble(taskId -> {
        final int start = 1 + taskId * sliceSize;
        final int end = (taskId + 1) * sliceSize;
        double sum = 0.0;
        for (int i = start; i <= end; ++i) {
          final double x = (i - 0.5) * delta;
          sum += 1.0 / (1.0 + x * x);
        }
        return sum;
      }).sum();
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out(Pi_ParallelStream_Batched.class, pi, n, elapseTime, numberOfTasks);
  }
  public static void main(final String[] args) {
    Pi_ParallelStream_Batched.execute(1);
    Pi_ParallelStream_Batched.execute(2);
    Pi_ParallelStream_Batched.execute(8);
    Pi_ParallelStream_Batched.execute(32);
  }
}
