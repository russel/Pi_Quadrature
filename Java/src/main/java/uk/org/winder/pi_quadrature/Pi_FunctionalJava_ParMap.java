/*
 *  Calculation of π using quadrature realized with an approached based on using parallel map from
 *  Functional Java.
 *
 *  Copyright © 2010–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import fj.F;
import fj.F2;
import fj.Unit;
import fj.data.Array;
import fj.control.parallel.ParModule;
import fj.control.parallel.Strategy;

public class Pi_FunctionalJava_ParMap {

  private static void execute(final int numberOfTasks) {
    final int n = 1000000000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final Array<Integer> inputData = Array.range(0, numberOfTasks);
    final F<Integer,Double> sliceCalculator = new F<Integer,Double>() {
      @Override public Double f(final Integer taskId) {
        final int start = 1 + taskId * sliceSize;
        final int end = (taskId + 1) * sliceSize;
        double sum = 0.0;
        for (int i = start; i <= end; ++i) {
          final double x = (i - 0.5) * delta;
          sum += 1.0 / (1.0 + x * x);
        }
        return sum;
      }
    };
    final F2<Double,Double,Double> add = new F2<Double,Double,Double>() {
      @Override public Double f(final Double a, final Double b) { return a + b; }
    };
    final Strategy<Unit> strategy = Strategy.simpleThreadStrategy();
    final double pi = 4.0 * delta * ParModule.parModule(strategy).parMap(inputData, sliceCalculator).claim().foldLeft(add, 0.0);
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Pi_FunctionalJava_ParMap", pi, n, elapseTime, numberOfTasks);
  }

  public static void main(final String[] args) {
    Pi_FunctionalJava_ParMap.execute(1);
    Pi_FunctionalJava_ParMap.execute(2);
    Pi_FunctionalJava_ParMap.execute(8);
    Pi_FunctionalJava_ParMap.execute(32);
  }
}
