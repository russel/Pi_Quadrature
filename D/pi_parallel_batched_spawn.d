/*
 *  A D program to calculate π using quadrature using multiple processes and message passing.
 *
 *  Copyright © 2010–2015  Russel Winder
 */

import std.algorithm: reduce;
import std.concurrency: Tid, thisTid, receiveOnly, send, spawn;
import std.range: iota;

import core.time: MonoTime;

import outputFunctions: output;

void partialSum(Tid parent, immutable int id, immutable int sliceSize, immutable double delta) {
  send(parent, reduce!((double t, int i){
        immutable x = (i - 0.5) * delta;
        return t + 1.0 / (1.0 + x * x);})
    (0.0, iota(1 + id * sliceSize, (id + 1) * sliceSize)));
}

void execute(immutable int numberOfTasks) {
  immutable n = 1000000000;
  immutable delta = 1.0 / n;
  immutable startTime = MonoTime.currTime;
  immutable sliceSize = n / numberOfTasks;
  foreach (immutable i; 0 .. numberOfTasks) { spawn(&partialSum, thisTid, i, sliceSize, delta); }
  immutable pi = 4.0 * delta * reduce!((t, i) => t + receiveOnly!double())(0.0, iota(0, numberOfTasks));
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
