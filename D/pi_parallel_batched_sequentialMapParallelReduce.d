/*
 *  A D program to calculate π using quadrature as a parallel reduction of sequential maps.
 *
 *  Copyright © 2010–2016  Russel Winder
 */

import std.algorithm: map, reduce;
import std.parallelism: taskPool;
import std.range: iota;
import std.typecons: Tuple, tuple;

import core.time: MonoTime;

import outputFunctions: output;

double partialSum(immutable Tuple!(int, int, double) data) {
	return reduce!((double t, int i){
			immutable x = (i - 0.5) * data[2];
			return t + 1.0 / (1.0 + x * x);})
	  (0.0, iota(1 + data[0] * data[1], (data[0] + 1) * data[1] + 1));
}

void execute(immutable int numberOfTasks) {
	immutable n = 1_000_000_000;
	immutable delta = 1.0 / n;
	immutable startTime = MonoTime.currTime;
	immutable sliceSize = n / numberOfTasks;
	//  There is a problem using a lambda function here.  David Simcha reports it is a consequence of issue
	//  5710 http://d.puremagic.com/issues/show_bug.cgi?id=5710.
	immutable pi = 4.0 * delta * taskPool.reduce!"a + b"(map!(partialSum)(
			map!(i => tuple(i, cast(int) sliceSize, cast(double) delta))(iota(numberOfTasks))));
	immutable elapseTime = (MonoTime.currTime - startTime).total!"hnsecs" * 100e-9;
	output(__FILE__, pi, n, elapseTime, numberOfTasks);
}

int main(immutable string[] args) {
	execute(1);
	execute(2);
	execute(8);
	execute(32);
	return 0;
}
