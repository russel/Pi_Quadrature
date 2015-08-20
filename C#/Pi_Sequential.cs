/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2009, 2011, 2015  Russel Winder
 */

using static Output;

public class Pi_CS_Sequential {

  public static void Main (string[] args) {
    const long n = 1000000000L;
    const double delta = 1.0 / n;
    long startTimeHundredsOfNanos = System.DateTime.Now.Ticks;
    double sum = 0.0;
    for (long i = 1L; i <= n; ++i) {
      double x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    double pi = 4.0 * delta * sum;
    double elapseTime = (System.DateTime.Now.Ticks - startTimeHundredsOfNanos) / 1e7;
    Output.output("Sequential", pi, n, elapseTime);
  }

}
