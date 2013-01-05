/*
 *  Support class for the Java/Groovy version of the computation.
 *
 *  Copyright © 2010–2011, 2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

public class ProcessSlice {
  private final int taskId;
  private final int sliceSize;
  private final double delta;

  public ProcessSlice(final int taskId, final int sliceSize, final double delta) {
    this.taskId = taskId;
    this.sliceSize = sliceSize;
    this.delta = delta;
  }

  public double compute() {
    final int start = 1 + taskId * sliceSize;
    final int end = (taskId + 1) * sliceSize;
    double sum = 0.0;
    for (int i = start; i <= end; ++i) {
      final double x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    return sum;
  }
}
