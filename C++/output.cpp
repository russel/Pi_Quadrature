/*
 *  Output functions for the C++ programs to calculate π using quadrature.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <iomanip>
#include <iostream>
#include <string>

#include "output.hpp"

void out(std::string const banner, double const pi, int const n, double const elapseTime) {
    std::cout << "==================== " << banner << '\n'
              << "\tpi = " << std::setprecision ( 18 ) << pi << '\n'
              << "\titeration count = " << n << '\n'
              << "\telapse = " << elapseTime << std::endl ;
}

void out(std::string const banner, double const pi, int const n, double const elapseTime, int const threadCount, int const processorCount) {
    out(banner + " -- thread count: " + std::to_string(threadCount), pi, n, elapseTime);
    std::cout << "\tprocessor count = " << processorCount << std::endl;
}
