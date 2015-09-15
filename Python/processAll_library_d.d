/*
 *  D functions to calculate π using quadrature.
 *
 *  Copyright © 2015  Russel Winder
 */

import core.runtime: Runtime;

import std.algorithm: map, reduce;
import std.parallelism: taskPool;
import std.range: iota;

extern(C)
double sequential(const int n, const double delta) {
    // This code does not need the D runtime to be initialized, but for consistency…
  Runtime.initialize();
  const pi = 4.0 * delta * reduce!(
        (double t, int i){ immutable x = (i - 0.5) * delta; return t + 1.0 / (1.0 + x * x); })(
        0.0, iota(1, n + 1));
  Runtime.terminate();
  return pi;
}


/*
 *  There is a problem using a lambda function here.  David Simcha reports it is a consequence of issue
 *  5710 http://d.puremagic.com/issues/show_bug.cgi?id=5710.

extern(C)
double parallel(const int n, const double delta) {
  // This code does not need the D runtime to be initialized, but for consistency…
  Runtime.initialize();
  const pi = 4.0 * delta * taskPool.reduce!(
        (double t, int i){ immutable x = (i - 0.5) * delta; return t + 1.0 / (1.0 + x * x); })(
        0.0, iota(1, n + 1));
  Runtime.terminate();
  return pi;
}

* so must use something much less efficient:
*/

extern(C)
double parallel(const int n, const double delta) {
    // This code does not need the D runtime to be initialized, but for consistency…
  Runtime.initialize();
  const pi = 4.0 * delta * taskPool.reduce!"a + b"(
      map!((int i){ immutable x = (i - 0.5) * delta; return 1.0 / (1.0 + x * x); })(iota(1, n + 1)));
  Runtime.terminate();
  return pi;
}
