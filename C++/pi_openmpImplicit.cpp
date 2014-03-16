/*
 *  A C++ program to calculate π using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2008–2011, 2013, 2014  Russel Winder
 */

#include <chrono>

#include <omp.h>

#include "output.hpp"

int main() {
  const auto n = 1000000000;
  const auto delta = 1.0 / n;
  const auto startTime = std::chrono::steady_clock::now();
  auto sum = 0.0;
#pragma omp parallel for reduction(+ : sum)
  for (auto i = 1; i <= n; ++i) {
    const auto x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  const auto pi = 4.0 * delta * sum;
  const auto elapseTime = std::chrono::steady_clock::now() - startTime;
  out("OpenMP Implicit", pi, n, elapseTime, 0, omp_get_num_procs());
  return 0;
}
