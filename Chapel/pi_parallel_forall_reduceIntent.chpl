/*
 *  A Chapel program to calculate π using quadrature as a forall with reduce intents.
 *
 *  Copyright © 2009–2015  Russel Winder
 */

use Time;
use Output;

proc main() {
  param n = 10000000; // 100 times fewer due to inappropriate use of forall.
  const delta = 1.0 / n;
  var timer:Timer;
  timer.start();
  var sum: real = 0.0;
  forall i in 1..n with (+ reduce sum) { sum += 1.0 / (1.0 + ((i - 0.5) * delta) ** 2); }
  const pi = 4.0 * delta * sum;
  timer.stop();
  output("Forall Reduce Intent", pi, n,  timer.elapsed());
}
