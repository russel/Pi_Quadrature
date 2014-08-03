/*
 *  A C function to calculate a slice of the overall calculation of π using quadrature.
 *
 *  Copyright © 2009–2011, 2013, 2014  Russel Winder
 */

double processSlice(int const id, int const sliceSize, double const delta) {
  int const start = 1 + id * sliceSize;
  int const end = (id + 1) * sliceSize;
  double sum = 0.0;
  for (int i = start; i <= end; ++i) {
    double const x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}
