#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with GPars actors.  Done with class(es).
 *
 *  Copyright © 2009-10 Russel Winder.
 */

@Grab ( group = 'org.codehaus.gpars' ,  module = 'gpars' , version = '0.10' )

import java.util.List

import groovyx.gpars.actor.Actor
import groovyx.gpars.actor.impl.RunnableBackedPooledActor

public class Pi_Groovy_GPars_ActorClass {

  private static class  ComputeActor extends RunnableBackedPooledActor {
    private long start
    private long end
    private double delta
    private Actor accumulator
    ComputeActor ( final long start , final long end , final double delta , final Actor accumulator ) {
      this.start = start
      this.end = end
      this.delta = delta
      this.accumulator = accumulator
    }
    @Override protected void act ( ) {
      double sum = 0.0d
      for ( long i = start ; i <= end ; ++i ) {
        double x = ( i - 0.5d ) * delta
        sum += 1.0d / ( 1.0d + x * x )
      }
      accumulator << sum
    }
  }
  
  private static class AccumulatorActor extends RunnableBackedPooledActor {
    private List<Actor> sources
    private double sum = 0.0d
    AccumulatorActor ( final List<Actor> s ) { sources = s }
    @Override protected void act ( ) { for ( s in sources ) { receive { sum +=  it } } }
    public double getSum ( ) { return sum }
  }

  private static void execute ( final int actorCount ) {
    final long n = 100000000l // 10 times fewer due to speed issues.
    final double delta = 1.0d / n
    final long sliceSize = n / actorCount
    final long startTimeNanos = System.nanoTime ( )
    final computors = [ ]
    final accumulator = new  AccumulatorActor ( computors )
    for ( i in 0l ..< actorCount ) { computors.add ( new ComputeActor ( 1l + i * sliceSize , ( i + 1l ) * sliceSize , delta , accumulator ) ) }
    accumulator.start ( )
    for ( c in computors ) { c.start ( ) }
    accumulator.join ( )
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
    println ( "==== Groovy GPars ActorClass pi = " + 4.0d * accumulator.sum * delta )
    println ( "==== Groovy GPars ActorClass iteration count = " + n )
    println ( "==== Groovy GPars ActorClass elapse = " + elapseTime )
    println ( "==== Groovy GPars ActorClass processor count = " + Runtime.runtime.availableProcessors ( ) ) ;
    println ( "==== Groovy GPars ActorClass actor count = " + actorCount )
  }

  public static void main ( final String[] args ) {
    execute ( 1 )
    println ( )
    execute ( 2 )
    println ( )
    execute ( 8 )
    println ( )
    execute ( 32 )
  }

}
