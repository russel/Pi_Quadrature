/*
 *  Calculation of π using quadrature realized with a sequential algorithm using a comprehension.
 *
 *  Copyright © 2012, 2013, 2016  Russel Winder
 */

import uk.org.winder.pi_quadrature.tools { output }

"Calculate π using quadrature realized with a sequential algorithm using a comprehension."
by("Russel Winder")
shared void run() {
	value n = 1_000_000_000;
	value delta = 1.0 / n;
	value startTime = system.nanoseconds;
	value pi = 4.0 * delta * sum{for (i in 1..n) 1.0 / (1.0 + ((i - 0.5) * delta) ^ 2)};
	value elapseTime = (system.nanoseconds - startTime) / 1.0e9;
	output("pi_sequential_comprehension_power", pi, n, elapseTime);
}
