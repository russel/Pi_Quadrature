/*
 *  A C++ function to calculate a slice of the overall calculation of π using quadrature.
 *
 *  Copyright © 2009–2011, 2013–2015  Russel Winder
 */

#include <boost/python.hpp>
#include <boost/range/irange.hpp>

double process_slice(int const id, int const sliceSize, double const delta) {
	auto const r = boost::irange(1 + id * sliceSize, (id + 1) * sliceSize);
	return std::accumulate(r.begin(), r.end(), 0.0, [=](double t, long i){
			auto const x = (i - 0.5) * delta;
			return t + 1.0 / (1.0 + x * x);
		});
}

BOOST_PYTHON_MODULE(processAll_extension_boost) {
	boost::python::def("process_slice", process_slice);
}
