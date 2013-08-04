#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature with a basic sequential algorithm realized via a map/reduce
 *  expression.
 *
 *  Copyright © 2011, 2013  Russel Winder <russel@winder.org.uk>
 */

/*
 *  Use Float not Decimal so as to get some form of sane performance -- Decimals are realized as
 *  java.math.BigDecimal which whilst accurate are extraordinarily slow compared to java.lang.Double which
 *  is how Floats are realized – actually it is double, but let's not worry about that.
 */

/*
 *  This gets ugly of course as real data structures are used by the map and reduce methods rather than some
 *  form of "lazy" iterable and thus the space requirement is linear in the number of rectangles, sufficient
 *  to cause an OOME in fact.  Sometime explicit iteration is just the right thing to do.
 */

class Main {
  static Void main() {
    n := 1000000//000 //  Any more than 10^6 leads to OOME
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks()
    pi := 4.0f * delta * (Float)((1..n).map |i -> Float|{
        x := (i - 0.5f) * delta
        return 1.0f / (1.0f + x * x)
      }.reduce(0.0f) |Float l, Float r -> Float|{return l + r})
    elapseTime := (sys::DateTime.nowTicks() - startTimeNanos) / 1e9f
    pi_quadrature_output::Output.out("Sequential Reduce", pi, n, elapseTime)
  }
}
