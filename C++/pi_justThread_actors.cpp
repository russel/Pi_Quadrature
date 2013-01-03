/*
 *  A C++ program to calculate π using quadrature.  This uses Anthony Williams' Just::Threads Pro library which
 *  is an implementation of the threads specification of C++11 and has realizations of actors and dataflow.
 *
 *  Copyright © 2011–2013  Russel Winder
 */

#include <memory>

#include <jss/actor.hpp>

#include "output.hpp"

#include "microsecondTime.h"

void execute(int const numberOfWorkerActors) {
    auto const n = 1000000000;
    auto const delta = 1.0 / n;
    auto const startTimeMicros = microsecondTime();
    auto const sliceSize = n / numberOfWorkerActors;
    jss::actor accumulator([&]() {
            auto sum = 0.0;
            for (auto i = 0; i < numberOfWorkerActors; ++i) { jss::actor::receive().match<double>([&](double d) { sum += d; }); }
            auto const pi = 4.0 * delta * sum;
            auto const elapseTime = ( microsecondTime() - startTimeMicros) / 1e6;
            out("Just::Thread Actors", pi, n, elapseTime, numberOfWorkerActors, std::thread::hardware_concurrency());
        });
    std::unique_ptr<const jss::actor> calculators [ numberOfWorkerActors ];
    for (auto index = 0; index < numberOfWorkerActors; ++index) {
        calculators[index] = std::unique_ptr<const jss::actor>(new jss::actor([&]() {
                    auto const start = 1 + index * sliceSize;
                    auto const end = (index + 1) * sliceSize;
                    auto sum = 0.0;
                    for (auto i = start; i < end; ++i) {
                        auto const x = (i - 0.5) * delta;
                        sum += 1.0 / (1.0 + x * x);
                    }
                    accumulator.send(sum);
                }));
    }
}

int main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32);
    return 0;
}
