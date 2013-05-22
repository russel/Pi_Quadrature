/*
 *  A C++ program to calculate π using quadrature.    This uses threads à la C++11 for parallelism.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <vector>
#include <future>
#include <numeric>

#include "output.hpp"

#include "microsecondTime.h"

double partialSum(int const id, int const sliceSize, double const delta) {
  auto const start = 1 + id * sliceSize;
  auto const end = (id + 1) * sliceSize;
  auto sum = 0.0;
  for (auto i = start; i <= end; ++i) {
    auto const x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}

void execute(const int numberOfThreads) {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTimeMicros = microsecondTime();
  auto const sliceSize = n / numberOfThreads;
  std::vector<std::shared_future<double>> futures;
  for (auto i = 0; i < numberOfThreads; ++i) {
    futures.push_back(std::async(std::launch::async, partialSum, i, sliceSize, delta));
  }
  auto const sum = std::accumulate(futures.begin(), futures.end(), 0.0, [](double a, std::shared_future<double>b) { return a + b.get();});
  auto const pi = 4.0 * delta * sum;
  auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
  out("Just::Thread Futures", pi, n, elapseTime, numberOfThreads, std::thread::hardware_concurrency());
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
