/*
 *  A D program to calculate π using quadrature as a sequential reduction of sequential maps.  This is really
 *  just here as a comparison against the various parallel versions.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

import std.algorithm;
import std.datetime;
import std.range;
import std.typecons;

import output_d;

double partialSum(immutable Tuple !(int, int, double) data) {
  immutable start = 1 + data[0] * data[1];
  immutable end = (data[0] + 1) * data[1];
  auto sum = 0.0;
  foreach (i; start .. end + 1) {
    immutable x = (i - 0.5) * data[2];
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}

void execute(immutable int numberOfTasks) {
  immutable n = 1000000000;
  immutable delta = 1.0 / n;
  StopWatch stopWatch;
  stopWatch.start();
  immutable sliceSize = n / numberOfTasks;
  immutable pi = 4.0 * delta * reduce!(( a, b) => a + b)(0.0, map!(partialSum)(
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
