/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2014  Russel Winder
 */

package uk.org.winder.pi_quadrature;

public class Pi_Sequential {

  public static void main(final String[] args) {
    final int n = 1_000_000_000;
    final double delta = 1.0 / n;
    final long startTimeNanos = System.nanoTime();
    double sum = 0.0;
    for (int i = 1; i <= n; ++i) {
      final double x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    final double pi = 4.0 * delta * sum;
    final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out(Pi_Sequential.class, pi, n, elapseTime);
  }
}
