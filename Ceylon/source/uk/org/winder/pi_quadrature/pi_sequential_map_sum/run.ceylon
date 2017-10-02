/*
*  Calculation of π using quadrature realized with a sequential algorithm using map and sum.
*
*  Copyright © 2014, 2016  Russel Winder
*/

import uk.org.winder.pi_quadrature.tools { output }

"Calculate π using quadrature realized with a sequential algorithm using map and sum."
by("Russel Winder")
shared void run() {
	value n = 1_000_000_000;
	value delta = 1.0 / n;
	value startTime = system.nanoseconds;
	value pi = 4.0 * delta * sum((1..n).map((Integer i) {
				value x = (i - 0.5) * delta;
				return 1.0 / (1.0 + x * x);
			}));
	value elapseTime = (system.nanoseconds - startTime) / 1.0e9;
	output("pi_sequential_map_sum", pi, n, elapseTime);
}
