/*
 *  Calculation of π using quadrature realized with an approached based on evolving values in an array with
 *  implicit parallelism.  This is material that probably won't actually be in Java 7.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import extra166y.CommonOps;
import extra166y.Ops;
import extra166y.ParallelDoubleArray;
import extra166y.ParallelDoubleArrayWithDoubleMapping;

public class Pi_ParallelArray {

    private static void execute(final int numberOfTasks) {
        final int n = 1000000000;
        final double delta = 1.0 / n;
        final long startTimeNanos = System.nanoTime();
        final int sliceSize = n / numberOfTasks;
        final ParallelDoubleArray initialArray = ParallelDoubleArray.create(numberOfTasks, ParallelDoubleArray.defaultExecutor());
        final ParallelDoubleArrayWithDoubleMapping finalArray = initialArray.withIndexedMapping(new Ops.IntAndDoubleToDouble() {
                @Override public double op(final int taskId, final double initialValue) {
                    final int start = 1 + taskId * sliceSize;
                    final int end = (taskId + 1) * sliceSize;
                    double sum = 0.0;
                    for (int i = start; i <= end; ++i) {
                        final double x = (i - 0.5) * delta;
                        sum += 1.0 / (1.0 + x * x);
                    }
                    return sum;
                }
            });
        final double pi = 4.0 * delta * finalArray.reduce(CommonOps.doubleAdder(), 0.0);
        final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
        Output.out("Pi_ParallelArray", pi, n, elapseTime, numberOfTasks);
    }
    public static void main(final String[] args) {
        Pi_ParallelArray.execute(1);
        Pi_ParallelArray.execute(2);
        Pi_ParallelArray.execute(8);
        Pi_ParallelArray.execute(32);
    }
}
