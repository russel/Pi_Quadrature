/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2012–2014  Russel Winder
 */

import uk.org.winder.pi_quadrature.tools { output }

"Calculate π using quadrature realized with a basic sequential algorithm."
by("Russel Winder")
shared void run() {
	value n = 100_000_000; // 10 times fewer than Java due to speed issues.
	value delta = 1.0 / n;
	value startTime = system.nanoseconds;
	variable Float sum = 0.0;
	for (i in 1..n) {
		value x = (i - 0.5) * delta;
		sum += 1.0 / (1.0 + x * x);
	}
	value pi = 4.0 * delta * sum;
	value elapseTime = (system.nanoseconds - startTime) / 1.0e9;
	output("pi_sequential_loop", pi, n, elapseTime);
}
