#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature with a basic sequential algorithm realized via a map/reduce
 *  expression.
 *
 *  Copyright © 2011, 2013, 2014  Russel Winder <russel@winder.org.uk>
 */

/*
 *  This gets very ugly as real data structures are used by the map and reduce methods rather than some form
 *  of "lazy" iterable.  Thus the space requirement is linear in the number of rectangles, sufficient to
 *  cause an OOME in fact.
 */

class Main {
  static Void main() {
    // Fantom 1.066, JDK8 on ancient twin Xeon, 100000, 0.2s; 10000000, 13s. Hummm…
    n := 10000000 //  Any more than 10^7 leads to OOME
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
