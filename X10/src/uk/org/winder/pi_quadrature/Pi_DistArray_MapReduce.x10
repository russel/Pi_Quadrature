/*
 *  Calculation of  π using quadrature realized with a data parallel algorithm.
 *
 *  Copyright © 2009–2012, 2014  Russel Winder <russel@winder.org.uk>
 */

package uk.org.winder.pi_quadrature;

import x10.array.DistArray_Block_1;
import x10.io.Console;
import x10.compiler.Native;

public class Pi_DistArray_MapReduce {
  private static def partialSum(id:long, sliceSize:long, delta:double):double {
    val start = 1 + id * sliceSize;
    val end = (id + 1) * sliceSize;
    var sum:double = 0.0;
    for (i in start..end) {
      val x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    return sum;
  }
  private static def execute(numberOfTasks:long):void {
    val n = 1000000000;
    val delta = 1.0 / n;
    val startTimeNanos = System.nanoTime();
    val sliceSize = n / numberOfTasks;
    val pg = PlaceGroup.WORLD;
    val source = new DistArray_Block_1[Long](numberOfTasks, pg, (i:long) => i);
    val partialSums = new DistArray_Block_1[Double](numberOfTasks, pg);
    source.map(partialSums, (id:long) => partialSum(id, sliceSize, delta));
    val pi = 4.0 * delta * partialSums.reduce((t:double, a:double) => t + a, 0.0);
    val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Parallel", pi, n, elapseTime, numberOfTasks);
  }
  @Native("java", "true")
  @Native("c++", "false")
  private static native def onJVM():Boolean;
  public static def main(args:Rail[String]):void {
    if (onJVM()) {
      // Warm up the JIT
      execute(32);
      execute(32);
      execute(32);
      Console.OUT.println("\nIgnore the above, it's the JIT warm up.\n");
    }
    execute(1);
    execute(2);
    execute(8);
    execute(32);
  }
}
