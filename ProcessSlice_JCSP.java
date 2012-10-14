/*
 *  Support class for the Java/Groovy version of the computation using JCSP.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import org.jcsp.lang.ChannelOutput;
import org.jcsp.lang.CSProcess;

public class ProcessSlice_JCSP implements CSProcess {
  private final int taskId;
  private final int sliceSize;
  private final double delta;
  private final ChannelOutput<Double> out;
  public ProcessSlice_JCSP(final int taskId, final int sliceSize, final double delta, final ChannelOutput<Double> out) {
    this.taskId = taskId;
    this.sliceSize = sliceSize;
    this.delta = delta;
    this.out = out;
  }
  @Override public void run() {
    final int start = 1 + taskId * sliceSize;
    final int end = (taskId + 1) * sliceSize;
    double sum = 0.0;
    for (int i = start; i <= end; ++i) {
      final double x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    out.write(sum);
  }
}
