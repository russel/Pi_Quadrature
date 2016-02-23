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

int main(immutable string[] args) {
  immutable n = 1_000_000_000;
  immutable delta = 1.0 / n;
  immutable startTime = MonoTime.currTime;
  /*
   *  There is a problem using a lambda function here.  David Simcha reports it is a consequence of issue
   *  5710 http://d.puremagic.com/issues/show_bug.cgi?id=5710.
   *
  const f = delegate double(double t, int i) {
    immutable x = (i - 0.5) * delta;
    return 1.0 / (1.0 + x * x);};
  immutable pi = 4.0 * delta * taskPool.reduce!(f)(0.0, iota(1, n + 1));
  *
  * So we use the less efficient map–reduce. It seems we must have the delegate as a literal, it cannot be
  * pulled out, to get the parallelism.
  */
  immutable pi = 4.0 * delta * taskPool.reduce!"a + b"(map!((int i){
        immutable x = (i - 0.5) * delta;
        return 1.0 / (1.0 + x * x);})
    (iota(1, n + 1)));
  immutable elapseTime = (MonoTime.currTime - startTime).total!"hnsecs" * 100e-9;
  output(__FILE__, pi, n, elapseTime);
  return 0;
}
