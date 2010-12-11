#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.  With Java computation.
 *
 *  Copyright © 2010 Russel Winder.
 */

//  This variant provided by Václav Pech by private email.

@Grab ( 'org.codehaus.gpars:gpars:0.11-beta-3' )

import groovyx.gpars.actor.Actors
import groovyx.gpars.actor.DefaultActor

final class AccumulatorActor extends DefaultActor {
  private final actorCount
  def sum
  def AccumulatorActor ( actorCount ) { this.actorCount = actorCount }
  @Override protected void act ( ) { handleMessage ( 0 , actorCount , 0.0d ) }
  void handleMessage ( final n , final max , final sum ) {
    if ( n < max ) {
      react { handleMessage ( n + 1 , max , sum + it ) }
    } else {
      this.sum = sum
      terminate ( )
    }
  }
}

void execute ( final int actorCount ) {
  final long n = 1000000000l
  final double delta = 1.0d / n
  private final long sliceSize = n / actorCount
  final long startTimeNanos = System.nanoTime ( )
  final accumulator = new AccumulatorActor ( actorCount )
  accumulator.start ( )
  for ( index in 0 ..< actorCount ) {
    final localIndex = index
    Actors.actor { accumulator << ( new ProcessSlice ( localIndex , sliceSize , delta ) ).compute ( ) }
  }
  accumulator.join ( )
  final double pi = 4.0d * accumulator.sum * delta
  final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( '==== Groovy/Java GPars ActorScript pi = ' + pi )
  println ( '==== Groovy/Java GPars ActorScript iteration count = ' + n )
  println ( '==== Groovy/Java GPars ActorScript elapse = ' + elapseTime )
  println ( '==== Groovy/Java GPars ActorScript processor count = ' + Runtime.runtime.availableProcessors ( ) )
  println ( '==== Groovy/Java GPars ActorScript actor count = ' + actorCount )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
