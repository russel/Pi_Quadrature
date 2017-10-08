/*
 *  A D program to calculate π using quadrature as a parallel reduce of individual expression evaluations
 *  with no manual batching.
 *
 *  Copyright © 2011–2017  Russel Winder
 */

import std.algorithm: map;
import std.parallelism: taskPool;
import std.range: iota;

import core.time: MonoTime;

import outputFunctions: output;

int main(immutable string[] args) {
	immutable n = 1_000_000_000;
	immutable delta = 1.0 / n;
	immutable startTime = MonoTime.currTime;
	immutable pi = 4.0 * delta * taskPool.reduce!"a + b"(map!((int i){
				immutable x = (i - 0.5) * delta;
				return 1.0 / (1.0 + x * x);})
		(iota(1, n + 1)));
	immutable elapseTime = (MonoTime.currTime - startTime).total!"hnsecs" * 100e-9;
	output(__FILE__, pi, n, elapseTime);
	return 0;
}
