/*
 *  Calculation of π using quadrature realized with an approached based on Java 8 streams.
 *
 *  Copyright © 2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.ArrayList;

public class Pi_SequentialStream {
  private static void execute(final int numberOfTasks) {
    final int n = 1000000000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final ArrayList<Integer> indexes = new ArrayList<>();
    for (int i = 0 ; i < numberOfTasks; ++i) { indexes.add(i); }
    final double pi = 4.0 * delta * indexes.stream().map((taskId) -> {
        final int start = 1 + taskId * sliceSize;
        final int end = (taskId + 1) * sliceSize;
        double sum = 0.0;
        for (int i = start; i <= end; ++i) {
          final double x = (i - 0.5) * delta;
          sum += 1.0 / (1.0 + x * x);
        }
        return sum;
      }).reduce((a, b) -> a + b).getAsDouble();
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Pi_SequentialStream", pi, n, elapseTime, numberOfTasks);
  }
  public static void main(final String[] args) {
    Pi_SequentialStream.execute(1);
    Pi_SequentialStream.execute(2);
    Pi_SequentialStream.execute(8);
    Pi_SequentialStream.execute(32);
  }
}
