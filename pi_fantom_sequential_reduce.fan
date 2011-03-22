#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature with a basic sequential algorithm realized via a map/reduce
 *  expression.  This gets ugly of course as the space requirement is horrendous, sufficient to cause an
 *  OOME in fact.  Sometime iteration is just the right thing to do.
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
    pi := 4.0f * delta * (Float) ( ( 1..n ).toList ( ).map | i -> Float | {
        x := ( i - 0.5f ) * delta
        return 1.0f / ( 1.0f + x * x )
      }.reduce ( 0.0f ) | Float l , Float r -> Float | { return l + r } )
    elapseTime := ( sys::DateTime.nowTicks ( ) - startTimeNanos ) / 1e9f
    echo ( "==== Fantom Sequential Reduce pi = " + pi )
    echo ( "==== Fantom Sequential Reduce iteration count = " + n ) 
    echo ( "==== Fantom Sequential Reduce elapse = " + elapseTime )
  }
}
