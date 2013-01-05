/*
 *  A C program to calculate π using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2008–2011, 2013  Russel Winder
 */

#include <omp.h>

#include "microsecondTime.h"
#include "output.h"

void execute(int const numberOfThreads) {
  int const n = 1000000000;
  double const delta = 1.0 / n;
  long long const startTimeMicros = microsecondTime();
  int const sliceSize =  n / numberOfThreads;
  double sum = 0.0;
#pragma omp parallel for reduction (+ : sum)
  for (int i = 0; i < numberOfThreads; ++i) {
    int const start = 1 + i * sliceSize;
    int const end = (i + 1) * sliceSize;
    for (int j = start; j <= end; ++j) {
      double const x = (j - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
  }
  double const pi = 4.0 * delta * sum;
  double const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
  outn("OpenMP Implicit", pi, n, elapseTime, numberOfThreads, omp_get_num_procs());
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
