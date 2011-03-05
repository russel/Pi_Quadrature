#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm realized with a range
 *  expression.
 *
 *  Copyright © 2011 Russel Winder
 */

/*
 *  Use Float not Decimal so as to get some form of sane performance -- Decimals are realized as
 *  java.math.BigDecimal which whilst accurate are extraordinarily slow compared to java.lang.Double which
 *  is how Floats are realized.
 */

class Main {
  static Void main ( ) {
    n := 1000000000
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks ( )
    sum := 0.0f
    ( 1..n ).each | i | {
      x := ( i - 0.5f ) * delta
      sum += 1.0f / ( 1.0f + x * x )
    }
    pi := 4.0f * sum * delta
    elapseTime := ( sys::DateTime.nowTicks ( ) - startTimeNanos ) / 1e9f
    echo ( "==== Fantom Sequential Range pi = " + pi )
    echo ( "==== Fantom Sequential Range iteration count = " + n ) 
    echo ( "==== Fantom Sequential Range elapse = " + elapseTime )
  }
}
