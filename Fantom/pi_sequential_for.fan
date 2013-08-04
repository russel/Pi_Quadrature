#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature with a basic sequential algorithm realized via for statement.
 *
 *  Copyright © 2011, 2013  Russel Winder <russel@winder.org.uk>
 */

/*
 *  Use Float not Decimal so as to get some form of sane performance -- Decimals are realized as
 *  java.math.BigDecimal which whilst accurate are extraordinarily slow compared to java.lang.Double which
 *  is how Floats are realized – actually it is double, but let's not worry about that.
 *
 *  Int type in Fantom is 64-bit and there is no 32-bit integer type.  This means Fantom running on the JVM
 *  will always suffer the obvious JVM JIT startup issue.  So the following code will be slow since the JIT
 *  does not kick in.
 */

class Main {
  static Void main() {
    n := 100000000 // 10 times fewer than Java due to speed issues.
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks()
    sum := 0.0f
    for (i := 1; i <= n; ++i) {
      x := (i - 0.5f) * delta
      sum += 1.0f / (1.0f + x * x)
    }
    pi := 4.0f * delta * sum
    elapseTime := (sys::DateTime.nowTicks() - startTimeNanos) / 1e9f
    pi_quadrature_output::Output.out("Sequential For", pi, n, elapseTime)
  }
}
