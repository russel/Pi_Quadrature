#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature with a basic sequential algorithm realized via a range expression.
 *
 *  Copyright © 2011, 2013  Russel Winder <russel@winder.org.uk>
 */

/*
 *  Use Float not Decimal so as to get some form of sane performance -- Decimals are realized as
 *  java.math.BigDecimal which whilst accurate are extraordinarily slow compared to java.lang.Double which
 *  is how Floats are realized.
 *
 *  Int type in Fantom is 64-bit and there is no 32-bit integer type.  This means Fantom running on the JVM
 *  will always suffer the obvious JVM JIT startup issue.  Using a closure for the tight loop allows the JIT
 *  to kick in so this code will be faster than a simple for loop!
 */

class Main {
  static Void main() {
    n := 1000000000
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks()
    sum := 0.0f
    (1 .. n).each |i|{
      x := (i - 0.5f) * delta
      sum += 1.0f / (1.0f + x * x)
    }
    pi := 4.0f * delta * sum
    elapseTime := (sys::DateTime.nowTicks() - startTimeNanos) / 1e9f
    Output.out("Fantom Sequential Range", pi, n, elapseTime)
  }
}
