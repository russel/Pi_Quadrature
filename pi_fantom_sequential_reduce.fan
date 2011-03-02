#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm realized with map/reduce
 *  expression.  This gets ugly of course as the space requirement is horrendous, sufficient to cause an
 *  OOME in fact.  Sometime iteration is just the right thing to do.
 *
 *  Copyright © 2011 Russel Winder
 */

class Main {
  static Void main ( ) {
    n := 1000000000
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks ( )
    Float sum := ( 1..n ).toList ( ).map | i -> Float | {
      x := ( i - 0.5f ) * delta
      return 1.0f / ( 1.0f + x * x )
    }.reduce ( 0.0f ) | Float l , Float r -> Float | { return l + r }
    pi := 4.0f * sum * delta
    elapseTime := ( sys::DateTime.nowTicks ( ) - startTimeNanos ) / 1e9f
    echo ( "==== Fantom Sequential Reduce pi = " + pi )
    echo ( "==== Fantom Sequential Reduce iteration count = " + n ) 
    echo ( "==== Fantom Sequential Reduce elapse = " + elapseTime )
  }
}
