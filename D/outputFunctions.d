/*
 *  Output functions for the D realizations of π by quadrature.
 *
 *  Copyright © 2012, 2014, 2015  Russel Winder
 */

import std.conv: to;
import std.parallelism: totalCPUs;
import std.stdio: writeln, writefln;

void output(const string banner, const double pi, const int n, const double elapseTime) {
	writeln("======================== " ~ banner);
	writefln("\tπ = %.18f", pi);
	writefln("\titeration count = %d", n);
	writefln("\telapse time = %f", elapseTime);
}

void output(const string banner, const double pi, const int n, const double elapseTime, const int numberOfTasks) {
	output(banner ~ ", task count: " ~ to!string(numberOfTasks), pi, n, elapseTime);
	writefln("\tnumber of processors = %d", totalCPUs);
}
