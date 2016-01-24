/*
 *  A Chapel program to calculate π using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009, 2011–2015  Russel Winder
 */

use Time;
use Output;

proc main() {
  param n = 1000000000;
  const delta = 1.0 / n;
  var timer:Timer;
  timer.start();
  const pi = 4.0 * delta * (+ reduce for i in 0..n do 1.0 / (1.0 + ((i - 0.5) * delta) ** 2));
  timer.stop();
  output("Sequential Reduce", pi, n, timer.elapsed());
}
