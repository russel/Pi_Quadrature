/*
 *  Output functions for the D realizations of π by quadrature.
 *
 *  Copyright © 2012  Russel Winder
 */

import std.conv;
import std.parallelism;
import std.stdio;

void output(string banner, double pi, int n, double elapseTime) {
  writeln("======================== " ~ banner);
  writefln("\tπ = %.18f", pi);
  writefln("\titeration count = %d", n);
  writefln("\telapse time = %f", elapseTime);
}

void output(string banner, double pi, int n, double elapseTime, int numberOfTasks) {
  output(banner ~ ", task count: " ~ to!string(numberOfTasks), pi, n, elapseTime);
  writefln("\tnumber of processors = %d", totalCPUs);
}
