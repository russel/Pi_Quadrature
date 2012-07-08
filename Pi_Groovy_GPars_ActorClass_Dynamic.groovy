#! /usr/bin/env groovy 

/*
 *  Calculation of π using quadrature realized with GPars actors.  Done with class(es).
 *
 *  Copyright © 2009–2012 Russel Winder.
 */

import java.util.List

import groovyx.gpars.actor.Actor
import groovyx.gpars.actor.DefaultActor
import groovyx.gpars.actor.DynamicDispatchActor

public class Pi_Groovy_GPars_ActorClass_Dynamic {

  private static class  ComputeActor extends DefaultActor {
    private int taskId
    private int sliceSize
    private double delta
    private Actor accumulator
    ComputeActor ( final int taskId , final int sliceSize , final double delta , final Actor accumulator ) {
      this.taskId = taskId
      this.sliceSize = sliceSize
      this.delta = delta
      this.accumulator = accumulator
    }
    @Override protected void act ( ) {
      final int start = 1i + taskId * sliceSize
      final int end = ( taskId + 1i ) * sliceSize 
      double sum = 0.0d
      for ( int i in start .. end ) {
        double x = ( i - 0.5d ) * delta
        sum += 1.0d / ( 1.0d + x * x )
      }
      accumulator << sum
    }
  }
  
  private static class AccumulatorActor extends DynamicDispatchActor {
    private List<Actor> sources
    private double sum = 0.0d
    private int count = 0i
    AccumulatorActor ( final List<Actor> s ) { sources = s }
    @Override protected void onMessage ( final Double result ) {
      sum +=  result
      if ( ++count == sources.size ( ) ) { terminate ( ) }
    }
    public double getSum ( ) { return sum }
  }

  private static void execute ( final int actorCount ) {
    final int n = 100000000i // 10 times fewer due to speed issues.
    final double delta = 1.0d / n
    final startTimeNanos = System.nanoTime ( )
    final int sliceSize = n / actorCount
    final computors = [ ]
    final accumulator = new  AccumulatorActor ( computors )
    for ( int i in 0i ..< actorCount ) { computors.add ( new ComputeActor ( i , sliceSize , delta , accumulator ) ) }
    accumulator.start ( )
    for ( c in computors ) { c.start ( ) }
    accumulator.join ( )
    final pi = 4.0d * delta * accumulator.sum
    final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
    Output.out ( getClass ( ).name , pi , n , elapseTime , actorCount )
  }

  public static void main ( final String[] args ) {
    execute 1
    execute 2
    execute 8
    execute 32
  }

}
