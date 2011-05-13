#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.  With Java computation.
 *
 *  Copyright © 2010--2011 Russel Winder.
 */

//  This variant provided by Václav Pech by private email.
//
//  There appears to be what looks like the use of recursion in AccumulatorClass.handleMessage.  This is
//  effectively tail recursion implemented in the GPars actor messager handler -- neither Java nor Groovy
//  support tail recursion of course.

@Grab ( 'org.codehaus.gpars:gpars:0.12-beta-1' )

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
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / actorCount
  final accumulator = new AccumulatorActor ( actorCount )
  accumulator.start ( )
  for ( index in 0 ..< actorCount ) {
    final localIndex = index
    Actors.actor { accumulator << ( new ProcessSlice ( localIndex , sliceSize , delta ) ).compute ( ) }
  }
  accumulator.join ( )
  final double pi = 4.0d * accumulator.sum * delta
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( '==== Groovy/Java GPars ActorScript VP pi = ' + pi )
  println ( '==== Groovy/Java GPars ActorScript VP iteration count = ' + n )
  println ( '==== Groovy/Java GPars ActorScript VP elapse = ' + elapseTime )
  println ( '==== Groovy/Java GPars ActorScript VP processor count = ' + Runtime.runtime.availableProcessors ( ) )
  println ( '==== Groovy/Java GPars ActorScript VP actor count = ' + actorCount )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
