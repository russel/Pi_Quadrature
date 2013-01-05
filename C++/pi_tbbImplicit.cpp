/*
 *  A C program to calculate π using quadrature as a TBB implemented algorithm.
 *
 *  Copyright © 2009–2012, 2013  Russel Winder
 */

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_reduce.h"
#include "tbb/mutex.h"

#include "output.hpp"

#include "microsecondTime.h"

tbb::mutex joinMutex;

class partialSum {
 private:
  double const delta;
  double sum;
 public:
  partialSum(double const d): delta(d), sum(0.0) {}
  partialSum(partialSum const & x, tbb::split): delta(x.delta), sum(0.0) {}
  void operator()(tbb::blocked_range<long> const & range) {
    for (auto i = range.begin(); i != range.end(); ++i) {
      auto const x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
  }
  void join(partialSum const & x) {
    tbb::mutex::scoped_lock lock(joinMutex);
    sum += x.sum;
  }
  double getSum() { return sum; }
};

int main() {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTimeMicros = microsecondTime();
  tbb::task_scheduler_init tbb_initializer;
  partialSum accumulator(delta);
  tbb::parallel_reduce(tbb::blocked_range<long>(0, n), accumulator, tbb::auto_partitioner());
  auto const pi = 4.0 * delta * accumulator.getSum();
  auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
  out("TBB Implicit", pi, n, elapseTime, tbb::task_scheduler_init::default_num_threads(), tbb::task_scheduler_init::default_num_threads());
  return 0;
}
