/*
 *  A C program to calculate π using quadrature as a sequential algorithm.
 *
 *  Copyright © 2008–2011, 2013  Russel Winder
 */

#include "microsecondTime.h"
#include "output.h"

int main() {
    int const n = 1000000000;
    double const delta = 1.0 / n;
    long long const startTimeMicros = microsecondTime();
    double sum = 0.0;
    for (int i = 1; i <= n; ++i) {
        double const x = (i - 0.5) * delta;
        sum += 1.0 / (1.0 + x * x);
    }
    double const pi = 4.0 * delta * sum;
    double const elapseTime = (microsecondTime () - startTimeMicros) / 1e6;
    out("Sequential", pi, n, elapseTime);
    return 0;
}
