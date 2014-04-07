#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature with a basic sequential algorithm realized via a range expression.
 *
 *  Copyright © 2011, 2013, 2014  Russel Winder <russel@winder.org.uk>
 */

class Main {
  static Void main() {
    n := 100000000 // 10 times fewer than Java due to speed issues.
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks()
    sum := 0.0f
    (1 .. n).each |i|{
      x := (i - 0.5f) * delta
      sum += 1.0f / (1.0f + x * x)
    }
    pi := 4.0f * delta * sum
    elapseTime := (sys::DateTime.nowTicks() - startTimeNanos) / 1e9f
    pi_quadrature_output::Output.out("Sequential Range", pi, n, elapseTime)
  }
}
