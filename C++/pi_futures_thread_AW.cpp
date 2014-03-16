/*
 *  A C++ program to calculate π using quadrature.  This uses threads à la C++11 for parallelism.
 *
 *  This is a variant of my original by Anthony Williams which made it into the Just::Thread tests.
 *
 *  Copyright © 2009–2011, 2013, 2014  Russel Winder
 */

#include <chrono>
#include <thread>
#include <future>

#include "output.hpp"

double partialSum(int const id, int const sliceSize, double const delta) {
  auto const start = 1 + id * sliceSize;
  auto const end = (id + 1) * sliceSize;
  auto sum = 0.0;
  for (auto i = start; i <= end; ++i) {
    auto const x = (i - 0.5) * delta;
    sum += 1.0 / (1.0 + x * x);
  }
  return sum;
}

void execute(int const numberOfThreads) {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
  auto const sliceSize = n / numberOfThreads;
  std::packaged_task<double()> tasks[numberOfThreads];
  for (auto i = 0; i < numberOfThreads; ++i) {
    tasks[i] = std::packaged_task<double()>(std::bind(partialSum, i, sliceSize, delta));
    std::thread taskThread(std::ref(tasks[i]));
    taskThread.detach();
  }
  auto sum = 0.0;
  for (auto && task: tasks) { sum += task.get_future().get(); }
  auto const pi = 4.0 * delta * sum;
  auto const elapseTime = std::chrono::steady_clock::now() - startTime;
  out("Futures Thread AW", pi, n, elapseTime, numberOfThreads, std::thread::hardware_concurrency());
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
