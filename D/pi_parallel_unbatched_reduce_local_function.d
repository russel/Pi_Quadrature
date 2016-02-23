/*
 *  A D program to calculate π using quadrature as a parallel reduce of individual expression evaluations
 *  with no manual batching.
 *
 *  Copyright © 2011–2016  Russel Winder
 */

//  This version originally due to David Simcha, stemming from various emails on the various D email lists
//  and reified in the documentation for std.parallelism: http://dlang.org/phobos/std_parallelism.html,
//  http://cis.jhu.edu/~dsimcha/d/phobos/std_parallelism.html

import std.algorithm: map;
import std.parallelism: taskPool;
import std.range: iota;

import core.time: MonoTime;

import outputFunctions: output;

int main() {
  immutable n = 1_000_000_000;
  immutable delta = 1.0 / n;
  immutable startTime = MonoTime.currTime;
  real getTerm(int i) {
    immutable x = (i - 0.5) * delta;
    return delta / (1.0 + x * x);
  }
  immutable pi = 4.0 * taskPool.reduce!"a + b"(map!getTerm(iota(n)));
  immutable elapseTime = (MonoTime.currTime - startTime).total!"hnsecs" * 100e-9;
  output(__FILE__, pi, n, elapseTime);
  return 0;
}
