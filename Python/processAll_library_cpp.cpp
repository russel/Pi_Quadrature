/*
 *  C++ functions to calculate the main loop of π using quadrature.
 *
 *  Copyright © 2009–2011, 2013–2015, 2017  Russel Winder
 */

#include <vector>
#include <future>
#include <numeric>

extern "C"
double sequential(int const n, double const delta) {
	auto sum = 0.0;
	for (auto i = 1; i <= n; ++i) {
		auto const x = (i - 0.5) * delta;
		sum += 1.0 / (1.0 + x * x);
	}
	return 4.0 * delta * sum;
}

// Could use OpenMP or TBB in the following but use a pure C++17 solution to avoid any extra dependencies.

extern "C"
double parallel(int const n, double const delta) {
	int const number_of_threads =  std::thread::hardware_concurrency();
	auto const sliceSize = n / number_of_threads;
	std::vector<std::shared_future<double>> futures;
	for (auto i = 0; i < number_of_threads; ++i) {
		futures.push_back(std::async(std::launch::async,
			[=](int const id){
				auto sum = 0.0;
				for (auto i = 1 + id * sliceSize; i <= (id + 1) * sliceSize; ++i) {
					auto const x = (i - 0.5) * delta;
					sum += 1.0 / (1.0 + x * x);
				}
				return sum;
			},
			i
		));
	}
	auto const sum = std::accumulate(futures.begin(), futures.end(), 0.0,
		[](double a, std::shared_future<double>b){return a + b.get();}
	);
	return 4.0 * delta * sum;
}
