/*
 *  A D program to calculate π using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009–2014  Russel Winder
 */

import std.datetime: StopWatch;

import outputFunctions: output;

int main(immutable string[] args) {
  immutable n = 1000000000;
  immutable delta = 1.0 / n;
  StopWatch stopWatch;
  stopWatch.start();
  auto sum = 0.0;
  foreach (i; 1 .. n + 1) {
    immutable x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  immutable pi = 4.0 * delta * sum;
  stopWatch.stop();
  immutable elapseTime = stopWatch.peek().hnsecs * 100e-9;
  output(__FILE__, pi , n, elapseTime);
  return 0;
}
