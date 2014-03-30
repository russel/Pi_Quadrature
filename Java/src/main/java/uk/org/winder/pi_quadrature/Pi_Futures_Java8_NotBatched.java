/*
 *  Calculation of π using quadrature realized with a fork/join approach with threads and futures (hidden
 *  by using executors) to partition the problem and hence harness all processors available to the JVM.
 *
 *  Copyright © 2008–2014  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.concurrent.CompletableFuture;
import java.util.stream.IntStream;

public class Pi_Futures_Java8_NotBatched {

  private static void execute(final int numberOfTasks) {
    final int n = 1000000000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    final double pi = 4.0 * delta * IntStream.range(0, n).parallel().mapToObj((taskId)->
      CompletableFuture.supplyAsync(() -> {
        final double x = (taskId - 0.5) * delta;
        return 1.0 / (1.0 + x * x);
      })).mapToDouble(CompletableFuture::join).sum();
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Pi_Futures_Java8_NotBatched", pi, n, elapseTime, numberOfTasks);
  }

  public static void main(final String[] args) {
    Pi_Futures_Java8_NotBatched.execute(1);
    Pi_Futures_Java8_NotBatched.execute(2);
    Pi_Futures_Java8_NotBatched.execute(8);
    Pi_Futures_Java8_NotBatched.execute(32);
  }
}
