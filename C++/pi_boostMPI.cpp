/*
 *  A C++ program to calculate π using quadrature as an MPI-based algorithm.
 *
 *  Copyright © 2009–2011, 2013, 2014  Russel Winder
 */

#include<chrono>

#include <boost/mpi.hpp>

#include "output.hpp"

int main(int ac, char ** av) { // MPI requires writeable access to these parameters :-(
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
  boost::mpi::environment environment(ac, av);
  boost::mpi::communicator world;
  auto const nProcessors = world.size();
  auto const myId = world.rank();
  auto const sliceSize = n / nProcessors;
  auto const start = 1 + myId * sliceSize;
  auto const end = (myId + 1) * sliceSize;
  auto localSum = 0.0;
  for (auto i = start; i <= end; ++i) {
    auto const x = (i - 0.5) * delta;
    localSum += 1.0 / (1.0 + x * x);
  }
  auto sum = 0.0;
  boost::mpi::reduce(world, localSum, sum, std::plus<double>(), 0);
  if (myId == 0) {
    auto const pi = 4.0 * delta * sum;
    auto const elapseTime = std::chrono::steady_clock::now() - startTime;
    out("Boost MPI", pi, n, elapseTime, 0, nProcessors);
  }
  return 0;
}
