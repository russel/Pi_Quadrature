/*
 *  A C function to return the current microsecond timer.
 *
 *  Copyright © 2008, 2014  Russel Winder
 */

#include <sys/time.h>

#include "microsecondTime.h"

long long microsecondTime(void) {
  struct timeval value;
  gettimeofday(&value, 0);
  return ((long long)value.tv_sec) * 1000000ll + value.tv_usec;
}
