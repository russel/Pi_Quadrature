/*
 *  Calculation of π using quadrature realized with an approached based on Java 8 streams.
 *
 *  Copyright © 2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.stream.IntStream;

public class Pi_SequentialStream_Java8 {
  public static void main(final String[] args) {
    final int n = 1000000000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final double pi = 4.0 * delta * IntStream.range(0, n).mapToDouble(taskId -> {
        final double x = (i - 0.5) * delta;
        return 1.0 / (1.0 + x * x);
      }).sum();
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Pi_SequentialStream_Java8", pi, n, elapseTime, numberOfTasks);
  }
}
