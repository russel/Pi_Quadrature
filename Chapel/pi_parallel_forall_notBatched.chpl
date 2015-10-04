/*
 *  A Chapel program to calculate π using quadrature as an simple forall.
 *
 *  Copyright © 2009–2015  Russel Winder
 */

use Time;
use Output;

proc main() {
  param n = 1000000; // 1000 times fewer due to speed issues.
  const delta = 1.0 / n;
  var timer:Timer;
  timer.start();
  var sum: sync real = 0.0; // Synchronized variables make things very slow.
  // Should forall be used with such a large range? Almost certainly not.
  forall i in 1..n { sum += 1.0 / (1.0 + ((i - 0.5) * delta) ** 2); }
  const pi = 4.0 * delta * sum;
  timer.stop();
  output("Forall NotBatched", pi, n,  timer.elapsed());
}
