/*
 *  A D program to calculate π using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009–2016  Russel Winder
 */

import core.time: MonoTime;

import outputFunctions: output;

int main(immutable string[] args) {
	immutable n = 1_000_000_000;
	immutable delta = 1.0 / n;
	immutable startTime = MonoTime.currTime;
	auto sum = 0.0;
	foreach (immutable i; 1 .. n + 1) {
		immutable x = (i - 0.5) * delta;
		sum += 1.0 / (1.0 + x * x);
	}
	immutable pi = 4.0 * delta * sum;
	immutable elapseTime = (MonoTime.currTime - startTime).total!"hnsecs" * 100e-9;
	output(__FILE__, pi , n, elapseTime);
	return 0;
}
