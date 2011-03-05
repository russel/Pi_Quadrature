#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature realized with a data parallel algorithm realized with actors.
 *
 *  Copyright © 2011 Russel Winder
 */

/*
 *  Use Float not Decimal so as to get some form of sane performance -- Decimals are realized as
 *  java.math.BigDecimal which whilst accurate are extraordinarily slow compared to java.lang.Double which
 *  is how Floats are realized.
 */

class Main {
  static Void execute ( Int numberOfTasks ) {
    n := 1000000000
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks ( )
    sliceSize := n / numberOfTasks
    pool := concurrent::ActorPool ( )
    //  Sadly cannot nest closure definitions so we have to pull this out and assign it to a variable :-((
    partialSumEvaluator := | Int i -> Float | {
      start := 1 + i * sliceSize
      end := ( i + 1 ) * sliceSize
      sum := 0.0f
      ( start .. end ).each | j | {
        x := ( j - 0.5f ) * delta
        sum += 1.0f /  ( 1.0f + x * x )
      }
      return sum
    }
    /*
    concurrent::Future[] values := ( 0 ..< numberOfTasks ).toList ( ).map | i -> concurrent::Future | {
      return concurrent::Actor ( pool , partialSumEvaluator ).send ( i )
    }
    */
    Float sum := 1.0f // values.reduce ( 0.0f ) | Float l , concurrent::Future r -> Float | { return l + (Float) r.get ( ) }
    pi := 4.0f * sum * delta
    elapseTime := ( sys::DateTime.nowTicks ( ) - startTimeNanos ) / 1e9f
    echo ( "==== Fantom Futures pi = " + pi )
    echo ( "==== Fantom Futures iteration count = " + n ) 
    echo ( "==== Fantom Futures elapse = " + elapseTime )
  }
  static Void main ( ) {
    execute ( 1 ) 
    echo ( )
    execute ( 2 ) 
    echo ( )
    execute ( 8 ) 
    echo ( )
    execute ( 32 )
  }
}
