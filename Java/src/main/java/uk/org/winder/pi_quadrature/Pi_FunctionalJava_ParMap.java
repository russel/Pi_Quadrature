/*
 *  Calculation of π using quadrature realized with an approached based on using parallel map from
 *  FunctionalJava.
 *
 *  Copyright © 2010–2015  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import fj.data.Array;
import fj.control.parallel.ParModule;
import fj.control.parallel.Strategy;

public class Pi_FunctionalJava_ParMap {

  private static void execute(final int numberOfTasks) {
    final int n = 1_000_000_000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final double pi = 4.0 * delta * ParModule.parModule(Strategy.simpleThreadStrategy()).parMap(Array.range(0, numberOfTasks), taskId -> {
      final int start = 1 + taskId * sliceSize;
      final int end = (taskId + 1) * sliceSize;
      double sum = 0.0;
      for (int i = start; i <= end; ++i) {
        final double x = (i - 0.5) * delta;
        sum += 1.0 / (1.0 + x * x);
      }
      return sum;
    }).claim().foldLeft((a, b) -> a + b, 0.0);
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out(Pi_FunctionalJava_ParMap.class, pi, n, elapseTime, numberOfTasks);
  }

  public static void main(final String[] args) {
    Pi_FunctionalJava_ParMap.execute(1);
    Pi_FunctionalJava_ParMap.execute(2);
    Pi_FunctionalJava_ParMap.execute(8);
    Pi_FunctionalJava_ParMap.execute(32);
  }
}
