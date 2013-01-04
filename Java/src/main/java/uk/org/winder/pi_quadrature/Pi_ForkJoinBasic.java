/*
 *  Calculation of π using quadrature realized with a fork/join approach.  This uses a primitive array to
 *  store the futures.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

public class Pi_ForkJoinBasic {

    private static void execute(final int numberOfTasks) {
        final int n = 1000000000;
        final double delta = 1.0 / n;
        final long startTimeNanos = System.nanoTime();
        final int sliceSize = n / numberOfTasks;
        final ForkJoinPool pool = new ForkJoinPool(numberOfTasks);
        @SuppressWarnings("unchecked") final ForkJoinTask<Double>[] futures = new ForkJoinTask[numberOfTasks];
        for (int i = 0; i < numberOfTasks; ++i) {
            final int taskId = i;
            futures[taskId] = pool.submit(new Callable<Double>() {
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
                });
        }
        double sum = 0.0;
        for (final ForkJoinTask<Double> f : futures) {
            try { sum += f.get(); }
            catch(final InterruptedException ie) { throw new RuntimeException(ie); }
            catch(final ExecutionException ee) { throw new RuntimeException(ee); }
        }
        final double pi = 4.0 * delta * sum;
        final double elapseTime = (System.nanoTime() - startTimeNanos) / 1e9;
        //pool.shutdown();
        Output.out("Pi_ForkJoinBasic", pi, n, elapseTime, numberOfTasks);
    }

    public static void main(final String[] args) {
        Pi_ForkJoinBasic.execute(1);
        Pi_ForkJoinBasic.execute(2);
        Pi_ForkJoinBasic.execute(8);
        Pi_ForkJoinBasic.execute(32);
    }
}
