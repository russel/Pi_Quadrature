/*
 *  A C++ program to calculate π using quadrature.  This uses Anthony Williams' Just::Threads library which
 *  is an implementation of the threads specification of C++11.
 *
 *  This is a variant of pi_cpp_justThreadFutures.cpp from the Just::Thread tests -- Anthony took my
 *  examples and added them into the test suite but amended them a little.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <thread>
#include<future>

#include "output.hpp"

#include "microsecondTime.h"

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
    auto const startTimeMicros = microsecondTime();
    auto const sliceSize = n / numberOfThreads;
    std::packaged_task<double()> tasks[numberOfThreads];
    for (auto i = 0; i < numberOfThreads; ++i) {
        tasks[i] = std::packaged_task<double()>(std::bind(partialSum, i, sliceSize, delta));
        std::thread taskThread(std::ref(tasks[i]));
        taskThread.detach();
    }
    auto sum = 0.0;
    for (auto i = 0; i < numberOfThreads; ++i) { sum += tasks[i].get_future().get(); }
    auto const pi = 4.0 * delta * sum;
    auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
    out("Just::Thread Futures AW", pi, n, elapseTime, numberOfThreads, std::thread::hardware_concurrency());
}

int main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32);
    return 0;
}
