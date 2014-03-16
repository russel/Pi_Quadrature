/*
 *  Output functions for the C++ programs to calculate π using quadrature.
 *
 *  Copyright © 2009–2011, 2013, 2014  Russel Winder
 */

#if !defined(OUTPUT)
#define OUTPUT

#include <chrono>
#include <string>

void out(std::string const banner, double const pi, int const n, std::chrono::steady_clock::duration const elapseTime);
void out(std::string const banner, double const pi, int const n, std::chrono::steady_clock::duration const elapseTime, int const threadCount, int const processorCount);

#endif
