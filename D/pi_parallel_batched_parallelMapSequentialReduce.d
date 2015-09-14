/*
 *  A D program to calculate π using quadrature as a sequential reduction of parallel maps.
 *
 *  Copyright © 2010–2015  Russel Winder
 */

import std.algorithm: reduce;
import std.datetime: StopWatch;
import std.parallelism: map, taskPool;
import std.range: iota;
import std.typecons: Tuple, tuple;

import outputFunctions: output;

double partialSum(immutable Tuple!(int, int, double) data) {
  return reduce!(delegate double(double t, int i) {
      immutable x = (i - 0.5) * data[2];
      return t + 1.0 / (1.0 + x * x);})
    (0.0, iota(1 + data[0] * data[1], (data[0] + 1) * data[1] + 1));
}

void execute(immutable int numberOfTasks) {
  immutable n = 1000000000;
  immutable delta = 1.0 / n;
  StopWatch stopWatch;
  stopWatch.start();
  immutable sliceSize = n / numberOfTasks;
  immutable pi = 4.0 * delta * reduce!"a + b"(0.0, taskPool.amap!partialSum(
    map!(i => tuple(i, cast(int) sliceSize, cast(double) delta))(iota(numberOfTasks))));
  stopWatch.stop();
  immutable elapseTime = stopWatch.peek().hnsecs * 100e-9;
  output(__FILE__, pi, n, elapseTime, numberOfTasks);
}

int main(immutable string[] args) {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
