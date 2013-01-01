/**
 * The partial sum function for the Groovy codes as a statically compiled function and as a dynamically
 * compiled function.
 *
 *  Copyright © 2012 Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic static double staticCompile(final int taskId, final int sliceSize, final double delta) {
  final int start = 1i + taskId * sliceSize
  final int end = (taskId + 1i) * sliceSize
  double sum = 0.0d
  for (int i = start; i <= end; ++i) {
    final double x = (i - 0.5d) * delta
    sum += 1.0d / (1.0d + x * x)
  }
  sum
}

static double dynamicCompile(final int taskId, final int sliceSize, final double delta) {
  final int start = 1i + taskId * sliceSize
  final int end = (taskId + 1i) * sliceSize
  double sum = 0.0d
  for (int i = start; i <= end; ++i) {
    final double x = (i - 0.5d) * delta
    sum += 1.0d / (1.0d + x * x)
  }
  sum
}
