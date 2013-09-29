/*
 *  Calculation of  π using quadrature realized with a data parallel algorithm.
 *
 *  Copyright © 2009–2012   Russel Winder <russel@winder.org.uk>
 */

package uk.org.winder.pi_quadrature;

import x10.io.Console;

public class Pi_Parallel {
  private static def execute(numberOfTasks:int):void {
    val n = 1000000000;
    val delta = 1.0 / n;
    val startTimeNanos = System.nanoTime();
    val sliceSize = n / numberOfTasks;
    val pi = 4.0 * delta * finish(Reducible.SumReducer[Double]()) {
      for (0..(numberOfTasks - 1)) async {
    	val start = 1 + id * sliceSize;
    	val end = (id + 1) * sliceSize;
    	var sum:double = 0.0;
    	for (var i:long = start; i < end; ++i) {
    	  val x = (i - 0.5) * delta;
    	  sum += 1.0 / (1.0 + x * x);
    	}
        offer sum;
      }
    };
    val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Output.out("Parallel", pi, n, elapseTime, numberOfTasks);
  }
  public static def main(args:Rail[String]):void {
    execute(1);
    Console.OUT.println();
    execute(2);
    Console.OUT.println();
    execute(8);
    Console.OUT.println();
    execute(32);
  }
}
