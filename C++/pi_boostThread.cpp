/*
 *  A C++ program to calculate π using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <boost/thread/thread.hpp>

#include "output.hpp"

#include "microsecondTime.h"

double sum;
boost::mutex sumMutex;

class PartialSum {
 private:
  int const id;
  int const sliceSize;
  double const delta;
 public:
  PartialSum(int const i, int const s, double const d)
    : id(i), sliceSize(s), delta(d) {}
  void operator()() {
    auto const start = 1 + id * sliceSize;
    auto const end = (id + 1) * sliceSize;
    auto localSum = 0.0;
    for (auto i = start; i <= end; ++i) {
      auto const x = ( i - 0.5) * delta;
      localSum += 1.0 / ( 1.0 + x * x);
    }
    boost::mutex::scoped_lock lock(sumMutex);
    sum += localSum;
  }
};

void execute(int const numberOfThreads) {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTimeMicros = microsecondTime();
  auto const sliceSize = n / numberOfThreads;
  boost::thread_group threads;
  sum = 0.0;
  for (auto i = 0; i < numberOfThreads; ++i) { threads.create_thread(PartialSum(i, sliceSize, delta)); }
  threads.join_all();
  auto const pi = 4.0 * delta * sum;
  auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
  out("Boost.Thread", pi, n, elapseTime, numberOfThreads, boost::thread::hardware_concurrency());
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
