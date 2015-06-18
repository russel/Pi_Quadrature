/*
 *  A D function to calculate a slice of the overall calculation of π using quadrature.
 *
 *  Copyright © 2014  Russel Winder
 */

extern(C)
double processSlice(const int id, const int sliceSize, const double delta) {
  immutable start = 1 + id * sliceSize;
  immutable end = (id + 1) * sliceSize;
  auto sum = 0.0;
  foreach (int i; start..end) {
    immutable x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}
