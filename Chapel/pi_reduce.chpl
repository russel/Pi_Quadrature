/*
 *  A Chapel program to calculate Pi using quadrature as a parallel reduce-based algorithm.
 *
 *  Copyright © 2009--2013  Russel Winder
 */

use Time;
use Output;

proc execute(numberOfTasks:int) {
  param n:int(64) = 1000000000;
  const delta:real = 1.0 / n;
  var timer:Timer;
  timer.start();
  const sliceSize:int(64) = n / numberOfTasks;
  const eachProcessor:domain(1) = {0 ..(numberOfTasks - 1)};
  proc partialSum(const id:int):real {
    const start:int(64) = 1 + id * sliceSize;
    const end:int(64) = (id + 1) * sliceSize;
    var sum:real = 0.0;
    for i in start .. end {
      sum += 1.0 / (1.0 + ((i - 0.5) * delta) ** 2);
    }
    return sum;
  }
  const pi = 4.0 * delta * (+ reduce [i in eachProcessor] partialSum(i));
  timer.stop();
  output_more("Chapel Reduce", pi, n,  timer.elapsed(), numberOfTasks);
}

proc main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
}
