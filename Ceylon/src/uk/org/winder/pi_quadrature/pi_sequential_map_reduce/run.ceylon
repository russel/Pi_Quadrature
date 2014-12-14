/*
 *  Calculation of π using quadrature realized with a sequential algorithm using map and reduce.
 *
 *  Copyright © 2014  Russel Winder
 */

import uk.org.winder.pi_quadrature.tools { output }

"Calculate π using quadrature realized with a sequential algorithm using map and reduce."
by("Russel Winder")
shared void run() {
  value n = 100_000_000; // 10 times fewer than Java due to speed issues.
  value delta = 1.0 / n;
  value startTime = system.nanoseconds;
  value pi = 4.0 * delta * (1..n).map((Integer i) {
    value x = (i - 0.5) * delta;
    return 1.0 / (1.0 + x * x);
  }).reduce(plus<Float>);
  value elapseTime = (system.nanoseconds - startTime) / 1.0e9;
  output("pi_sequential_loop", pi, n, elapseTime);
}
