#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with GPars actors.  Done with class(es).
 *
 *  Copyright © 2009–2011 Russel Winder.
 */

//@Grab ( 'org.codehaus.gpars:gpars:0.12' )
@Grab ( 'org.codehaus.gpars:gpars:1.0-SNAPSHOT' )

import java.util.List

import groovyx.gpars.actor.Actor
import groovyx.gpars.actor.DefaultActor
import groovyx.gpars.actor.DynamicDispatchActor

public class Pi_GroovyJava_GPars_ActorClass {

  private static class  ComputeActor extends DefaultActor {
    private ProcessSlice sliceProcessor
    private Actor accumulator
    ComputeActor ( final int taskId , final int sliceSize , final double delta , final Actor accumulator ) {
      this.sliceProcessor = new ProcessSlice ( taskId , sliceSize , delta )
      this.accumulator = accumulator
    }
    @Override protected void act ( ) { accumulator << sliceProcessor.compute ( ) }
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
    final int n = 1000000000i
    final double delta = 1.0d / n
    final startTimeNanos = System.nanoTime ( )
    final int sliceSize = n / actorCount
    final computors = [ ]
    final accumulator = new  AccumulatorActor ( computors )
    for ( int i in 0i ..< actorCount ) { computors.add ( new ComputeActor ( i , sliceSize , delta , accumulator ) ) }
    accumulator.start ( )
    for ( c in computors ) { c.start ( ) }
    accumulator.join ( )
    final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
    println ( '==== GroovyJava GPars ActorClass pi = ' + 4.0d * accumulator.sum * delta )
    println ( '==== GroovyJava GPars ActorClass iteration count = ' + n )
    println ( '==== GroovyJava GPars ActorClass elapse = ' + elapseTime )
    println ( '==== GroovyJava GPars ActorClass processor count = ' + Runtime.runtime.availableProcessors ( ) ) ;
    println ( '==== GroovyJava GPars ActorClass actor count = ' + actorCount )
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
