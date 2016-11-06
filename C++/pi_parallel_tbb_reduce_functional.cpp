/*
 *  A C program to calculate π using quadrature as a TBB functional parallel reduce implemented algorithm.
 *
 *  Copyright © 2009–2012, 2013, 2014, 2016  Russel Winder
 */

#include <functional>
#include <chrono>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_reduce.h"
#include "tbb/mutex.h"

#include "output.hpp"

int main() {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
  tbb::task_scheduler_init tbb_initializer;
  auto const pi = 4.0 * delta * tbb::parallel_reduce(
		tbb::blocked_range<long>(0, n),
		0.0,
		[=](tbb::blocked_range<long> const & range, double value) -> double {
			// TBB does not support range-based for on ranges :-(
			//for (auto && i : range) {
			for (auto i = range.begin(); i != range.end(); ++i) {
				auto const x = (i - 0.5) * delta;
				value += 1.0 / (1.0 + x * x);
			}
			return value;
		},
		std::plus<double>()
	);
  auto const elapseTime = std::chrono::steady_clock::now() - startTime;
  out("TBB Parallel Reduce Functional", pi, n, elapseTime, tbb::task_scheduler_init::default_num_threads(), tbb::task_scheduler_init::default_num_threads());
  return 0;
}
