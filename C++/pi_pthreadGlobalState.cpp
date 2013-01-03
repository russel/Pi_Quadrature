/*
 *  A C program to calculate π using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <pthread.h>

#include "output.hpp"

#include "microsecondTime.h"

auto const n = 1000000000;
auto const delta = 1.0 / n;

int sliceSize ;

double sum;
pthread_mutex_t sumMutex;

/*
 *  On a 32-bit machine a void* and an int probably have the same size so we can simply pass a literal value
 *  and cast the type.  On a 64-bit machines things get a bit more complicated.  For the moment just use a
 *  long.  This code depends on GCC being used.
 */
#if defined(__LP64__)
typedef long integerFromVoidStar;
#else
typedef int integerFromVoidStar;
#endif

void * partialSum(void *const arg ) {
    auto const start = 1 + ((integerFromVoidStar) arg) * sliceSize;
    auto const end = (((integerFromVoidStar) arg) + 1) * sliceSize;
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
    auto const startTimeMicros = microsecondTime();
    sum = 0.0; // Only one thread at this point so safe to access without locking.
    sliceSize = n / numberOfThreads; // Only one thread at this point so safe to access without locking.
    pthread_mutex_init(&sumMutex , NULL);
    pthread_attr_t attributes;
    pthread_attr_init(&attributes);
    pthread_attr_setdetachstate(&attributes , PTHREAD_CREATE_JOINABLE);
    pthread_t threads[numberOfThreads];
    for (auto i = 0; i < numberOfThreads; ++i) {
        if (pthread_create(&threads[i] , &attributes , partialSum , (void *) i) != 0) { exit(1); }
    }
    pthread_attr_destroy(&attributes);
    int status;
    for (auto i = 0; i < numberOfThreads; ++i) { pthread_join(threads[i] , (void **) &status); }
    auto const pi = 4.0 * delta * sum;
    auto const elapseTime = (microsecondTime() - startTimeMicros) / 1e6;
    out("PThread Global State", pi, n, elapseTime, numberOfThreads, 0);
}

int main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32);
    return 0;
}
