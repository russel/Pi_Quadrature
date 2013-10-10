/*
 *  Calculation of π using quadrature realized with a sequential algorithm using a comprehension.
 *
 *  Copyright © 2012, 2013  Russel Winder
 */

import uk.org.winder.pi_quadrature.tools { output }

"Calculate π using quadrature realized with a sequential algorithm using a comprehension."
by("Russel Winder")
void run() {
	value n = 100000000; // 10 times fewer than Java due to speed issues.
	value delta = 1.0 / n;
	value startTime = process.nanoseconds;
	value pi = 4.0 * delta * sum({for (i in 1..n) ((Integer i){
		value x = (i - 0.5) * delta;
		return 1.0 / (1.0 + x * x);
	})(i)});
	value elapseTime = (process.nanoseconds - startTime) / 1.0e9;
	output("pi_ceylon_comprehension", pi, n, elapseTime);
}
