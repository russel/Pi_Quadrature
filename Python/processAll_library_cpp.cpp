/*
 *  A C++ function to calculate the main loop of π using quadrature.
 *
 *  Copyright © 2009–2011, 2013–2015  Russel Winder
 */

extern "C"
double sequential(int const n, double const delta) {
  auto sum = 0.0;
  for (auto i = 1; i <= n; ++i) {
    auto const x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}

extern "C"
double parallel(int const n, double const delta) {
	return 0.0;
}
