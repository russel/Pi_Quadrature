/*
 *  A C++ function to calculate a slice of the overall calculation of π using quadrature.
 *
 *  Copyright © 2009–2011, 2013–2015  Russel Winder
 */

extern "C"
double processSlice(int const id, int const sliceSize, double const delta) {
  auto sum = 0.0;
  for (auto i = 1 + id * sliceSize; i <= (id + 1) * sliceSize; ++i) {
    auto const x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}
