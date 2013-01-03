/*
 *  A C program to calculate π using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2011, 2013  Russel Winder
 */

#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

#include "microsecondTime.h"
#include "output.h"

#define n 1000000000
double const delta = 1.0 / n;

int sliceSize ;

double sum;
pthread_mutex_t sumMutex;

void * partialSum(void *const arg ) {
    int const start = 1 + ((long) arg) * sliceSize;
    int const end = (((long) arg) + 1) * sliceSize;
    double localSum = 0.0;
    for (int i = start; i <= end; ++i) {
        double const x = (i - 0.5) * delta;
        localSum += 1.0 / (1.0 + x * x);
    }
    pthread_mutex_lock(&sumMutex);
    sum += localSum;
    pthread_mutex_unlock(&sumMutex);
    pthread_exit((void *) 0);
    return 0;
}

void execute (const int numberOfThreads) {
    long long const startTimeMicros = microsecondTime();
    sum = 0.0; // Only one thread at this point so safe to access without locking.
    sliceSize = n / numberOfThreads; // Only one thread at this point so safe to access without locking.
    pthread_mutex_init(&sumMutex, NULL);
    pthread_attr_t attributes;
    pthread_attr_init(&attributes);
    pthread_attr_setdetachstate(&attributes, PTHREAD_CREATE_JOINABLE);
    pthread_t threads[numberOfThreads];
    for (int i = 0; i < numberOfThreads; ++i) {
        if (pthread_create (&threads[i], &attributes, partialSum, (void *) i) != 0) { exit(1); }
    }
    pthread_attr_destroy(&attributes);
    int status;
    for (int i = 0; i < numberOfThreads; ++i) { pthread_join (threads[i], (void **) &status); }
    double const pi = 4.0 * delta * sum;
    double const elapseTime = (microsecondTime () - startTimeMicros) / 1e6;
    outn("PThread Global", pi, n, elapseTime, numberOfThreads, sysconf(_SC_NPROCESSORS_ONLN));
}

int main() {
    execute(1);
    execute(2);
    execute(8);
    execute(32);
    return 0;
}
