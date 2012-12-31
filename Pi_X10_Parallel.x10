/*
 *  Calculation of Pi using quadrature realized with a data parallel algorithm.
 *
 *  As at versions 2.0.[23], X10 fails to terminate compilation in the presence of the copyright symbol as a
 *  UTF-8 encoded Unicode codepoint.  See XTENLANG-1236, http://jira.codehaus.org/browse/XTENLANG-1236.
 *  Version 2.1.0 terminates but gives an error message of bizarre nature.
 *
 *  Copyright (c) 2009--2012  Russel Winder
 */

import x10.io.Console;

public class Pi_X10_Parallel {
  private static def execute(numberOfTasks:int):void {
    val n = 1000000000l;
    val delta = 1.0 / n;
    val startTimeNanos = System.nanoTime();
    val sliceSize = n / numberOfTasks;
    val computeSlice = (p:Point) => {
      val id = p(0);
      val start = 1 + id * sliceSize;
      val end = (id + 1) * sliceSize;
      var sum:double = 0.0;
      for (var i:long = start; i < end; ++i) {
        val x = (i - 0.5) * delta;
        sum += 1.0 / (1.0 + x * x);
      }
      sum
    };
    val sums = DistArray.make[Double](Dist.makeBlock(0 .. (numberOfTasks - 1)) , computeSlice);
    val pi = 4.0 * delta * sums.reduce(Double.+ , 0.0);
    val elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
    Console.OUT.println("==== X10 Parallel pi = " + pi);
    Console.OUT.println("==== X10 Parallel iteration count = " + n);
    Console.OUT.println("==== X10 Parallel elapse = " + elapseTime);
    Console.OUT.println("==== X10 Parallel task count = " + numberOfTasks);
  }
  public static def main(args:Array[String]):void {
    execute(1);
    Console.OUT.println();
    execute(2);
    Console.OUT.println();
    execute(8);
    Console.OUT.println();
    execute(32);
  }
}
