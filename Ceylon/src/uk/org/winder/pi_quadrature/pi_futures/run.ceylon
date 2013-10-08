/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2012, 2013  Russel Winder
 */

import java.lang { System { nanoTime } }
import java.util.concurrent { Callable, ScheduledThreadPoolExecutor }
import uk.org.winder.pi_quadrature.tools { outputN }

Callable<Float> createCallable(Integer id, Integer sliceSize, Float delta) {
	object callable satisfies Callable<Float> {
		shared actual Float call() {
			value start = 1 + id * sliceSize;
      value end = (id + 1) * sliceSize;
      variable Float sum = 0.0;
      for (i in start..end) {
        value x = (i - 0.5) * delta;
        sum += 1.0 / (1.0 + x * x);
      }
      return sum;
    }
	}
	return callable;
}

void execute(Integer numberOfTasks) {
  value n = 100000000; // 10 times fewer than Java due to speed issues.
  value delta = 1.0 / n;
  value startTime = nanoTime();
  Integer sliceSize = n / numberOfTasks;
  value executor = ScheduledThreadPoolExecutor(numberOfTasks);
  value futures = [for (i in 0..numberOfTasks) executor.submit(createCallable(i, sliceSize, delta))];
  value pi = 4.0 * delta * sum({for (f in futures) f.get()});
  value elapseTime = (nanoTime() - startTime) / 1.0e9;
  outputN("pi_ceylon_futures", pi, n, elapseTime, numberOfTasks);
}

"Caclculate π using quadrature realized with a basic sequential algorithm."
by("Russel Winder")
void run() {
	execute(1);
	execute(2);
	execute(8);
	execute(32);
}
