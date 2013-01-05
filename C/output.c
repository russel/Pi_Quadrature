/*
 *  Output functions for C program to calculate π using quadrature.
 *
 *  Copyright © 2008–2011, 2013  Russel Winder
 */

#include <stdio.h>

#include "output.h"

void out(char const *const banner, double const pi, int const n, double const elapseTime) {
  printf("==================== %s\n", banner);
  printf("\tpi = %.18lf\n", pi);
  printf("\titeration count = %d\n" ,  n ) ;
  printf("\telapse time = %lf\n" , elapseTime ) ;
}

void outn(char const *const banner, double const pi, int const n, double const elapseTime, int const threadCount, int const processorCount) {
  char buffer[256];
  sprintf(buffer, "%s -- thread count: %d", banner, threadCount);
  out(buffer, pi, n, elapseTime);
  printf("\tprocessor count = %d\n", processorCount);
}
