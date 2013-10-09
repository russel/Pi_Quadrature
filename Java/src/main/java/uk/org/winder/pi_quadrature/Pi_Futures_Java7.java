/*
 *  Calculation of π using quadrature realized with a fork/join approach with threads and futures (hidden
 *  by using executors) to partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2008–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.ArrayList;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledThreadPoolExecutor;

public class Pi_Futures_Java7 {

  private static void execute(final int numberOfTasks) {
    final int n = 1000000000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final int sliceSize = n / numberOfTasks;
    final ExecutorService executor = new ScheduledThreadPoolExecutor(numberOfTasks);
    final ArrayList<Future<Double>> futures = new ArrayList<>();
    for (int i = 0; i < numberOfTasks; ++i) {
      final int taskId = i;
      futures.add(executor.submit(new Callable<Double>() {
            @Override public Double call() {
              final int start = 1 + taskId * sliceSize;
              final int end = (taskId + 1) * sliceSize;
              double sum = 0.0;
              for (int i = start; i <= end; ++i) {
                final double x = (i - 0.5) * delta;
                sum += 1.0 / (1.0 + x * x);
              }
              return sum;
            }
          }));
    }
    double sum = 0.0;
    for (final Future<Double> f : futures) {
      try { sum += f.get(); }
      catch (InterruptedException | ExecutionException e) { throw new RuntimeException(e); }
    }
    executor.shutdown();
    final double pi = 4.0 * delta * sum;
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Pi_Futures_Java7", pi, n, elapseTime, numberOfTasks);
  }

  public static void main(final String[] args) {
    Pi_Futures_Java7.execute(1);
    Pi_Futures_Java7.execute(2);
    Pi_Futures_Java7.execute(8);
    Pi_Futures_Java7.execute(32);
  }
}
