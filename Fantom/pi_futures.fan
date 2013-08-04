#! /usr/bin/env fan

/*
 *  Calculation of Pi using quadrature realized with a data parallel algorithm realized with actors.
 *
 *  Copyright © 2011, 2013  Russel Winder <russel@winder.org.uk>
 */

/*
 *  Use Float not Decimal so as to get some form of sane performance -- Decimals are realized as
 *  java.math.BigDecimal which whilst accurate are extraordinarily slow compared to java.lang.Double which
 *  is how Floats are realized – actually it is double, but let's not worry about that.
 */

class Main {
  static Void execute(Int numberOfTasks) {
    n := 1000000000
    delta := 1.0f / n
    startTimeNanos := sys::DateTime.nowTicks()
    sliceSize := n / numberOfTasks
    pool := concurrent::ActorPool ( )
    partialSumEvaluator := |Int id -> Float|{
      start := 1 + id * sliceSize
      end := (id + 1) * sliceSize
      sum := 0.0f
      (start .. end).each |i| {
        x := (i - 0.5f) * delta
        sum += 1.0f / (1.0f + x * x)
      }
      return sum
    }
    values := (0 ..< numberOfTasks).map |i -> concurrent::Future|{
      return concurrent::Actor(pool, partialSumEvaluator).send(i)
    }
    pi := 4.0f * delta * (Float) values.reduce(0.0f) |Float l, concurrent::Future r -> Float|{return l + (Float) r.get()}
    elapseTime := (sys::DateTime.nowTicks() - startTimeNanos) / 1e9f
    pi_quadrature_output::Output.outN("Parallel Futures", pi, n, elapseTime, numberOfTasks)
  }

  static Void main() {
    execute(1)
    execute(2)
    execute(8)
    execute(32)
  }
}
