/*
 *  A D program to calculate π using quadrature as a threads-based approach.
 *
 *  Copyright © 2009–2015  Russel Winder
 */

import std.datetime: StopWatch;

import core.atomic: atomicOp;
import core.thread: Thread;

import outputFunctions: output;

shared double sum;

void partialSum(immutable int id, immutable int sliceSize, immutable double delta) {
  immutable start = 1 + id * sliceSize;
  immutable end = (id + 1) * sliceSize;
  auto localSum = 0.0;
  foreach (immutable i; start .. end + 1) {
    immutable x = (i - 0.5) * delta;
    localSum += 1.0 / (1.0 + x * x);
  }
  atomicOp!"+="(sum, localSum);
}

void execute(immutable int numberOfThreads) {
  immutable n = 1000000000;
  immutable delta = 1.0 / n;
  StopWatch stopWatch;
  stopWatch.start();
  immutable sliceSize = n / numberOfThreads;
  sum = 0.0;
  auto threads = new Thread[numberOfThreads];
  foreach (immutable i; 0 .. numberOfThreads) {
    auto closedPartialSum() {
      immutable ii = i;
      return delegate(){partialSum(ii, sliceSize, delta);};
    }
    threads[i] = new Thread(closedPartialSum);
  }
  foreach (thread; threads) { thread.start(); }
  foreach (thread; threads) { thread.join(); }
  immutable pi = 4.0 * delta * sum;
  stopWatch.stop();
  immutable elapseTime = stopWatch.peek().hnsecs * 100e-9;
  output(__FILE__, pi, n, elapseTime, numberOfThreads);
}

int main(immutable string[] args) {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
