/*
 *  A C++ program to calculate π using quadrature with parallelism provided using C++11 threads.
 *
 *  Copyright © 2009–2011, 2013, 2014  Russel Winder
 */

#include <chrono>
#include <mutex>
#include <thread>

#include "output.hpp"

double sum;
std::mutex sumMutex;

void partialSum(int const id, int const sliceSize, double const delta) {
  auto const start = 1 + id * sliceSize;
  auto const end = (id + 1) * sliceSize;
  auto localSum = 0.0;
  for (auto i = start; i <= end; ++i) {
    auto const x = (i - 0.5) * delta;
    localSum += 1.0 / (1.0 + x * x);
  }
  std::lock_guard<std::mutex> lock(sumMutex);
  sum += localSum;
}

void execute(int const numberOfThreads) {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
  auto const sliceSize = n / numberOfThreads;
  std::thread threads[numberOfThreads];
  sum = 0.0;
  for (auto i = 0; i < numberOfThreads; ++i) { threads[i] = std::thread(partialSum, i, sliceSize, delta); }
  for (auto && thread: threads) { thread.join(); }
  auto const pi = 4.0 * delta * sum;
  auto const elapseTime = std::chrono::steady_clock::now() - startTime;
  out("Threads Global State", pi, n, elapseTime, numberOfThreads, std::thread::hardware_concurrency());
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
