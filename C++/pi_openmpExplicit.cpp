/*
 *  A C program to calculate π using quadrature as an OpenMP annotated algorithm.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <omp.h>

#include "output.hpp"

#include "microsecondTime.h"

void execute(int const numberOfThreads) {
    auto const n = 1000000000;
    auto const delta = 1.0 / n;
    auto const startTimeMicros = microsecondTime();
    auto const sliceSize = n / numberOfThreads;
    auto sum = 0.0;
#pragma omp parallel for reduction(+: sum)
    for (auto i = 0; i < numberOfThreads; ++i) {
        auto const start = 1 + i * sliceSize;
        auto const end = (i + 1) * sliceSize;
        for (auto j = start; j <= end; ++j) {
            auto const x = (j - 0.5) * delta;
            sum += 1.0 / (1.0 + x * x);
        }
    }
    auto const pi = 4.0 * delta * sum;
    auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
    out("OpenMP Explicit", pi, n, elapseTime, numberOfThreads, omp_get_num_procs());
}

int main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32);
    return 0;
}
