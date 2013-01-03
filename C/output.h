/*
 *  Output functions for C program to calculate Pi using quadrature.
 *
 *  Copyright © 2013  Russel Winder
 */

#if !defined(OUTPUT)
#define OUTPUT

void out(char const *const banner, double const pi, int const n, double const elapseTime);
void outn(char const *const banner, double const pi, int const n, double const elapseTime, int const threadCount, int const processorCount);

#endif
