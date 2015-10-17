/*
 *  Calculation of  π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2009–2015  Russel Winder <russel@winder.org.uk>
 */

package uk.org.winder.pi_quadrature;

import x10.compiler.Native;

public class Pi_Sequential_Loop {

  public static def execute():void {
    val n = 1000000000;
    val delta = 1.0 / n;
    val startTimeNanos = System.nanoTime();
    var sum:double = 0.0;
    for (i in 1..n) {
      val x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    val pi = 4.0 * delta * sum;
    val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Sequential", pi, n, elapseTime);
  }

  @Native("java", "true")
  @Native("c++", "false")
  private static native def onJVM():Boolean;

  public static def main(args:Rail[String]):void {
    if (onJVM()) {
      // Warm up the JIT
      execute();
      execute();
      execute();
      Console.OUT.println("\nIgnore the above, it's the JIT warm up.\n");
    }
    execute();
  }

}
