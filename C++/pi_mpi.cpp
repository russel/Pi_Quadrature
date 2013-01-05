/*
 *  A C++ program to calculate π using quadrature.  This is an SPMD realization using OpenMPI.
 *
 *  Copyright © 2008–2011, 2013  Russel Winder
 */

#include <mpi.h>

#include "output.hpp"

#include "microsecondTime.h"

int main(int ac , char ** av) { // MPI requires writeable access to these parameters!
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTimeMicros = microsecondTime();
  MPI::Init(ac , av);
  auto const nProcessors = MPI::COMM_WORLD.Get_size();
  auto const myId = MPI::COMM_WORLD.Get_rank();
  //std::cout << "Node of rank " << myId << " working." << std::endl;
  auto const sliceSize = n / nProcessors;
  auto const start = 1 + myId * sliceSize;
  auto const end = (myId + 1) * sliceSize;
  auto localSum = 0.0;
  for (auto i = start; i <= end; ++i) {
    auto const x = (i - 0.5) * delta;
    localSum += 1.0 / (1.0 + x * x);
  }
  auto sum = 0.0;
  MPI::COMM_WORLD.Reduce(&localSum , &sum , 1 , MPI::DOUBLE , MPI::SUM , 0);
  MPI::Finalize();
  if (myId == 0) {
    auto const pi = 4.0 * delta * sum;
    auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
    out("MPI", pi, n, elapseTime, 0, nProcessors);
  }
  return 0;
}
