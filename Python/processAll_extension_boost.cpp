/*
 *  A C++ function to calculate a the overall calculation of π using quadrature.
 *
 *  Copyright © 2009–2011, 2013–2015  Russel Winder
 */

#include <future>
#include <thread>
#include <vector>

#include <boost/python.hpp>
#include <boost/range/irange.hpp>

double sum_up(long const start, long const end, double const delta) {
	auto const r = boost::irange(start, end);
	return std::accumulate(r.begin(), r.end(), 0.0, [=](double t, long i) {
			auto const x = (i - 0.5) * delta;
			return t + 1.0 / (1.0 + x * x);
		});
}

double sequential(long const n, double const delta) {
  return 4.0 * delta * sum_up(1, n, delta);
}

double parallel(long const n, double const delta) {
  int const task_count = std::thread::hardware_concurrency();
	long const slice_size = n / task_count;
	std::vector<std::shared_future<double>> futures;
	for (auto i = 0; i < task_count; ++i) {
    futures.push_back(std::async(
																 std::launch::async,
																 [](int const id, long const slice_size, double const delta){
																	 return sum_up(1 + id * slice_size, (id + 1) * slice_size, delta);
																 },
																 i,
																 slice_size,
																 delta));
  }
  return 4.0 * delta * std::accumulate(
																			 futures.begin(),
																			 futures.end(),
																			 0.0,
																			 [](double a, std::shared_future<double> b) { return a + b.get();});
}

BOOST_PYTHON_MODULE(processAll_extension_boost) {
  boost::python::def("sequential", sequential);
  boost::python::def("parallel", parallel);
}
