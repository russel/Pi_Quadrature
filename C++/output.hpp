/*
 *  Output functions for the C++ programs to calculate π using quadrature.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#if !defined(OUTPUT)
#define OUTPUT

#include <string>

void out(std::string const banner, double const pi, int const n, double const elapseTime);
void out(std::string const banner, double const pi, int const n, double const elapseTime, int const threadCount, int const processorCount);

#endif
