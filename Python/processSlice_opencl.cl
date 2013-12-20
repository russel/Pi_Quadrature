// -*- mode:c; -*-

/*
 *  An OpenCL kernel for calculating slices of the π  by Quadrature problem.
 *
 *  Copyright © 2012  Russel Winder
 *
 *  Author: Russel Winder <russel@winder.org.uk>
 */

__kernel void processSlice(int sliceSize, double delta, __global double * results) {
  unsigned int const id = get_global_id(0);
  int const start = 1 + id * sliceSize;
  int const end = (id + 1) * sliceSize + 1;
  double sum = 0.0;
  for (int i = start; i < end; ++i) {
    double const x =  (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  results[id] = sum;
}
