/*
 *  A D program to calculate π using quadrature as a sequential reduce of individual expression evaluations
 *  with no manual batching. This is really just here as a comparison against the parallel version.
 *
 *  Copyright © 2011–2015  Russel Winder
 */

import std.algorithm: map, reduce;
import std.datetime: StopWatch;
import std.range: iota;

import outputFunctions: output;

int main(immutable string[] args) {
  immutable n = 1000000000;
  immutable delta = 1.0 / n;
  StopWatch stopWatch;
  stopWatch.start();
  const f = (double t, int i){
      immutable x = (i - 0.5) * delta;
      return t + 1.0 / (1.0 + x * x);};
  immutable pi = 4.0 * delta * reduce!(f)(0.0, iota(1, n + 1));
  stopWatch.stop();
  immutable elapseTime = stopWatch.peek().hnsecs * 100e-9;
  output(__FILE__, pi, n, elapseTime);
  return 0;
}
