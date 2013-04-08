/*
 *  Calculation of π using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2012, 2013  Russel Winder
 */

import java.lang { System { nanoTime } }

"A function to output stuff."
by "Russel Winder"
void output(String name, Float pi, Integer n, Float elapseTime) {
  print("================ ``name``");
  print("\tπ = ``pi``");
  print("\titeration count = ``n``");
  print("\telapse time = ``elapseTime``");
}

"The function that does the hard work."
by "Russel Winder"
void run() {
  value n = 100000000; // 10 times fewer than Java due to speed issues.
  value delta = 1.0 / n;
  value startTime = nanoTime();
  variable Float sum = 0.0;
  for (i in 1 .. n) {
    value x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  value pi = 4.0 * delta * sum;
  value elapseTime = (nanoTime() - startTime) / 1.0e9;
  output("pi_ceylon_sequential", pi, n, elapseTime);
}
