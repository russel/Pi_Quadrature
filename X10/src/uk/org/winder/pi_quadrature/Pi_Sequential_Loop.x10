/*
 *  Calculation of  π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2009–2014  Russel Winder <russel@winder.org.uk>
 */

package uk.org.winder.pi_quadrature;

public class Pi_Sequential_Loop {
  public static def main(args:Rail[String]):void {
    val n = 1000000000;
    val delta = 1.0 / n;
    val startTimeNanos = System.nanoTime();
    var sum:double = 0.0;
    for (var i:long = 1; i <= n; ++i) {
      val x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    val pi = 4.0 * delta * sum;
    val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Sequential", pi, n, elapseTime);
  }
}
