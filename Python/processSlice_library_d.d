/*
 *  A D function to calculate a slice of the overall calculation of π using quadrature.
 *
 *  Copyright © 2014–2017  Russel Winder
 */

import core.runtime: Runtime;

import std.algorithm: reduce;
import std.range: iota;

extern(C)
double processSlice(const int id, const int sliceSize, const double delta) {
	return reduce!(
		(double t, int i){
			immutable x = (i - 0.5) * delta;
			return t + 1.0 / (1.0 + x * x);
		})(
		0.0,
		iota(1 + id * sliceSize,  (id + 1) * sliceSize)
	);
}

// Have to set up the D runtime.

extern (C) {
	version(LDC) {
		pragma(LDC_global_crt_ctor, 0);
		void initRuntime() {
			import core.runtime: Runtime;
			Runtime.initialize();
		}
		pragma(LDC_global_crt_dtor, 0);
		void deinitRuntime() {
			import core.runtime: Runtime;
			Runtime.terminate();
		}
	}
}
