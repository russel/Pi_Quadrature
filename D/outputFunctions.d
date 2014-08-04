/*
 *  Output functions for the D realizations of π by quadrature.
 *
 *  Copyright © 2012, 2014  Russel Winder
 */

import std.conv: to;
import std.parallelism: totalCPUs;
import std.stdio: writeln, writefln;

void output(in string banner, in double pi, in int n, in double elapseTime) {
  writeln("======================== " ~ banner);
  writefln("\tπ = %.18f", pi);
  writefln("\titeration count = %d", n);
  writefln("\telapse time = %f", elapseTime);
}

void output(in string banner, in double pi, in int n, in double elapseTime, in int numberOfTasks) {
  output(banner ~ ", task count: " ~ to!string(numberOfTasks), pi, n, elapseTime);
  writefln("\tnumber of processors = %d", totalCPUs);
}
