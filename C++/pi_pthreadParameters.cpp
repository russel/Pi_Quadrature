/*
 *  A C program to calculate π using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <pthread.h>

#include "output.hpp"

#include "microsecondTime.h"

double sum;
pthread_mutex_t sumMutex;

struct CalculationParameters {
  int id;
  int sliceSize;
  double delta;
  CalculationParameters(): id(0l), sliceSize(0l), delta(0.0) { }
  CalculationParameters(int const i, int const s, double const d): id(i), sliceSize(s), delta(d) { }
  CalculationParameters(CalculationParameters const & x) {
    id = x.id;
    sliceSize = x.sliceSize;
    delta = x.delta;
  }
};

void * partialSum(void *const arg ) {
  auto const start = 1 + ((CalculationParameters *const) arg)->id * ((CalculationParameters *const) arg)->sliceSize;
  auto const end = (((CalculationParameters *const) arg)->id + 1) * ((CalculationParameters *const) arg)->sliceSize;
  auto const delta = ((CalculationParameters *const) arg)->delta;
  auto localSum = 0.0;
  for (auto i = start; i <= end; ++i) {
    auto const x = (i - 0.5) * delta;
    localSum += 1.0 / (1.0 + x * x);
  }
  pthread_mutex_lock(&sumMutex);
  sum += localSum;
  pthread_mutex_unlock(&sumMutex);
  pthread_exit((void *) 0);
  return 0;
}

void execute(int const numberOfThreads) {
  auto const n = 1000000000;
  auto const delta = 1.0 / n;
  auto const startTimeMicros = microsecondTime();
  auto const sliceSize = n / numberOfThreads;
  pthread_mutex_init(&sumMutex, NULL);
  pthread_attr_t attributes;
  pthread_attr_init(&attributes);
  pthread_attr_setdetachstate(&attributes, PTHREAD_CREATE_JOINABLE);
  sum = 0.0; // Only one thread at this point so safe to access without locking.
  pthread_t threads[numberOfThreads];
  CalculationParameters parameters[numberOfThreads];
  for (auto i = 0; i < numberOfThreads; ++i) {
    parameters[i] = CalculationParameters(i, sliceSize, delta);
    if (pthread_create(&threads[i], &attributes, partialSum, (void *) &parameters[i]) != 0) { exit(1); }
  }
  pthread_attr_destroy(&attributes);
  int status;
  for (auto i = 0; i < numberOfThreads; ++i) { pthread_join(threads[i], (void **) &status); }
  auto const pi = 4.0 * delta * sum;
  auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
  out("PThread Parameters", pi, n, elapseTime, numberOfThreads, 0);
}

int main() {
  execute(1);
  execute(2);
  execute(8);
  execute(32);
  return 0;
}
