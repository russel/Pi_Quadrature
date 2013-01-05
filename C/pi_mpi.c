/*
 *  A C program to calculate π using quadrature.  This is an SPMD realization using OpenMPI.
 *
 *  Copyright © 2008–2011, 2013  Russel Winder
 */

#include <mpi.h>

#include "microsecondTime.h"
#include "output.h"

int main(int ac, char ** av) { // MPI requires write access to these parameters!
  int const n = 1000000000;
  double const delta = 1.0 / n;
  int nProcessors, myId;
  long long const startTimeMicros = microsecondTime();
  MPI_Init(&ac, &av);
  MPI_Comm_size(MPI_COMM_WORLD, &nProcessors);
  MPI_Comm_rank(MPI_COMM_WORLD, &myId);
  int const sliceSize = n / nProcessors;
  int const start = 1 + myId * sliceSize;
  int const end = (myId + 1) * sliceSize;
  double localSum = 0.0;
  for (int i = start; i <= end; ++i) {
    double const x = (i - 0.5) * delta;
    localSum += 1.0 / (1.0 + x * x);
  }
  double sum;
  MPI_Reduce(&localSum, &sum, 1, MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
  MPI_Finalize();
  if (myId == 0) {
    double const pi = 4.0 * delta * sum;
    double const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
    outn("MPI", pi, n, elapseTime, 0, nProcessors);
  }
  return 0;
}
