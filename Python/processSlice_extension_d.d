/*
 *  A D function to calculate a slice of the overall calculation of π using quadrature.
 *
 *  Copyright © 2015  Russel Winder
 */

import pyd.pyd;

import std.algorithm: reduce;
import std.range: iota;

double processSlice(const int id, const int sliceSize, const double delta) {
  const sum = reduce!((double t, int i){
      immutable x = (i - 0.5) * delta;
      return t + 1.0 / (1.0 + x * x);
    })
    (0.0, iota(1 + id * sliceSize,  (id + 1) * sliceSize));
  return sum;
}

extern(C) void PydMain() {
  def!(processSlice)();
  module_init();
}
