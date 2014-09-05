// -*- mode:c; -*-

/*
 *  An OpenCL kernel for calculating slices of the π by Quadrature problem.
 *
 *  Copyright © 2012, 2014  Russel Winder
 *
 *  Author: Russel Winder <russel@winder.org.uk>
 */

// Quadro FX 570 card on Anglides only supports 32-bit operations, hence float not double.

__kernel void processSlice(int const id, float const delta, __global float * results) {
  float const x =  (id - 0.5) * delta;
  results[id] = 1.0 / (1.0 + x * x);
}
