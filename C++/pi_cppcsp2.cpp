/*
 *  A C++ program to calculate π using quadrature as a CSP implemented algorithm.
 *
 *  Copyright © 2010–2011, 2013, 2014  Russel Winder
 */

#include <chrono>
#include <vector>

#include <cppcsp/cppcsp.h>

#include "output.hpp"

class Compute: public csp::CSProcess {
 private:
  int const id;
  int const sliceSize;
  double const delta;
  csp::Chanout<double> const chanout;
 public:
  Compute(int const i, int const s, double const d, csp::Chanout<double> const c)
    : id(i), sliceSize(s), delta(d), chanout(c) {
  }
  void run() {
    auto const start = 1 + id * sliceSize;
    auto const end = (id + 1) * sliceSize;
    auto sum = 0.0;
    for (auto i = start; i <= end; ++i) {
      auto const x = (i - 0.5) * delta;
      sum += 1.0 / (1.0 + x * x);
    }
    chanout << sum;
  }
};

class Accumulate: public csp::CSProcess {
 private:
  int const n;
  int const numberOfProcesses;
  std::chrono::steady_clock::time_point const startTime;
  int const sliceSize;
  double const delta;
  csp::AltChanin<double> const chanin;
 public:
  Accumulate(int const ni, int const np, std::chrono::steady_clock::time_point const st, int const s, double const d, csp::AltChanin<double> const c)
    : n(ni), numberOfProcesses(np), startTime(st), sliceSize(s), delta(d), chanin(c) {
  }
  void run() {
    auto sum = 0.0;
    for (auto i = 0; i < numberOfProcesses; ++i) { double s; chanin >> s; sum += s; }
    auto const pi = 4.0 * delta * sum;
    auto const elapseTime = std::chrono::steady_clock::now() - startTime;
    out("C++ CSP 2", pi, n, elapseTime, numberOfProcesses, 0);
  }
};

void execute(int const numberOfProcesses) {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTime = std::chrono::steady_clock::now();
  auto const sliceSize = n / numberOfProcesses;
  csp::Start_CPPCSP();
  csp::Any2OneChannel<double> results;
  std::vector<csp::CSProcessPtr> processes;
  processes.push_back(new Accumulate(n, numberOfProcesses, startTime, sliceSize, delta, results.reader()));
  for (auto i = 0; i < numberOfProcesses; ++i) { processes.push_back(new Compute(i, sliceSize, delta, results.writer())); }
  csp::Run(csp::InParallel(processes.begin(), processes.end()));
  csp::End_CPPCSP();
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
