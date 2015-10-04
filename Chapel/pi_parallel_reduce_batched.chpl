/*
 *  A Chapel program to calculate π using quadrature as a parallel reduce-based algorithm.
 *
 *  Copyright © 2009–2015  Russel Winder
 */

use Time;
use Output;

proc execute(numberOfTasks:int) {
  param n = 1000000000;
  const delta = 1.0 / n;
  var timer:Timer;
  timer.start();
  const sliceSize = n / numberOfTasks;
  const eachProcessor = {0 ..(numberOfTasks - 1)};
  proc partialSum(const id:int):real {
    // Ensure this is a sequential calculation, do not use reduce!
    var sum: real = 0.0;
    for i in (1 + id * sliceSize)..((id + 1) * sliceSize) {
      sum += 1.0 / (1.0 + ((i - 0.5) * delta) ** 2);
    }
    return sum;
  }
  const pi = 4.0 * delta * (+ reduce [i in eachProcessor] partialSum(i));
  timer.stop();
  output_more("Reduce Batched", pi, n,  timer.elapsed(), numberOfTasks);
}

proc main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
}
