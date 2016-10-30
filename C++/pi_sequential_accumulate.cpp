/*
 *  A C++ program to calculate π using quadrature as a sequential algorithm.
 *
 *  Copyright © 2008–2011, 2013, 2014  Russel Winder
 */

#include <chrono>
#include <numeric>

#include <boost/range/irange.hpp>

#include "output.hpp"

int main() {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
	auto const range = boost::irange<double>(1, n);
  auto const pi = 4.0 * delta * std::accumulate(range.begin(), range.end(), 0.0, [=](double t, int i) {
			auto const x = (i - 0.5) * delta;
			return t + 1.0 / (1.0 + x * x);
		});
  auto const elapseTime = std::chrono::steady_clock::now() - startTime;
  out("Sequential Accumulate", pi, n, elapseTime);
  return 0;
}
