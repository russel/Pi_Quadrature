/*
 *  A Chapel program to calculate π using quadrature as a reduce-based algorithm.
 *
 *  Copyright © 2015  Russel Winder
 */

use Time;
use Output;

proc main() {
  param n = 1000000000;
  const delta = 1.0 / n;
  var timer:Timer;
  timer.start();
  const pi = 4.0 * delta * (+ reduce [i in 1..n] 1.0 / (1.0 + ((i - 0.5) * delta) ** 2));
  timer.stop();
  output("Reduce Not Batched", pi, n, timer.elapsed());
}
