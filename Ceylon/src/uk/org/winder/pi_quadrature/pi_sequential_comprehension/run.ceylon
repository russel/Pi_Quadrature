/*
 *  Calculation of π using quadrature realized with a sequential algorithm using a comprehension.
 *
 *  Copyright © 2012, 2013  Russel Winder
 */

import java.lang { System { nanoTime } }
import uk.org.winder.pi_quadrature.tools { output }

Float f(Integer i, Float delta) {
	value x = (i - 0.5) * delta;
  return 1.0 / (1.0 + x * x);
}

"Calculate π using quadrature realized with a sequential algorithm using a comprehension."
by("Russel Winder")
void run() {
  value n = 100000000; // 10 times fewer than Java due to speed issues.
  value delta = 1.0 / n;
  value startTime = nanoTime();
  value pi = 4.0 * delta * sum({for (i in 1..n) f(i, delta)});
  value elapseTime = (nanoTime() - startTime) / 1.0e9;
  output("pi_ceylon_comprehension", pi, n, elapseTime);
}
