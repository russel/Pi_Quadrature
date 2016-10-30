/*
 *  A C program to calculate π using quadrature as a TBB imperative parallel reduce implemented algorithm.
 *
 *  Copyright © 2009–2012, 2013, 2014, 2016  Russel Winder
 */

#include <chrono>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_reduce.h"
#include "tbb/mutex.h"

#include "output.hpp"

class partialSum {
 private:
  double const delta;
  double sum;
	tbb::mutex sumMutex;

 public:
  partialSum(double const d): delta(d), sum(0.0) {}
  partialSum(partialSum const & x, tbb::split): delta(x.delta), sum(0.0) {}
  void operator()(tbb::blocked_range<long> const & range) {
		// TBB does not support range-based for on ranges :-(
    //for (auto && i : range) {
		for (auto i = range.begin(); i != range.end(); ++i) {
      auto const x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
  }
  void join(partialSum const & x) {
    tbb::mutex::scoped_lock lock(sumMutex);
    sum += x.sum;
  }
  double getSum() { return sum; }
};

int main() {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
  tbb::task_scheduler_init tbb_initializer;
  partialSum accumulator(delta);
  tbb::parallel_reduce(tbb::blocked_range<long>(0, n), accumulator);
  auto const pi = 4.0 * delta * accumulator.getSum();
  auto const elapseTime = std::chrono::steady_clock::now() - startTime;
  out("TBB Parallel Reduce Imperative", pi, n, elapseTime, tbb::task_scheduler_init::default_num_threads(), tbb::task_scheduler_init::default_num_threads());
  return 0;
}
