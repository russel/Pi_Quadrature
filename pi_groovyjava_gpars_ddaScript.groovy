#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with an GPars actor based algorithm.
 *
 *  Copyright © 2010 Russel Winder
 */

//  This code provided by Václav Pech but private email.  It is based on the pre-existing Groovy/Java
//  examples and makes use of the Java coded ProcessSlice class.

@Grab ( group = 'org.codehaus.gpars' , module = 'gpars' , version = '0.11-beta-1' )

import groovyx.gpars.actor.Actors
import groovyx.gpars.actor.DynamicDispatchActor

final class DDAAccumulator extends DynamicDispatchActor {
  private final actorCount
  def sum = 0.0
  private def counter = 0
  def DDAAccumulator ( actorCount ) { this.actorCount = actorCount }
  void onMessage ( value ) {
    sum += value
    counter += 1
    if ( counter == actorCount ) { terminate ( ) }
  }
}

void execute ( final int actorCount ) {
  final long n = 1000000000l
  final double delta = 1.0d / n
  private final long sliceSize = n / actorCount
  final long startTimeNanos = System.nanoTime ( )
  final accumulator = new DDAAccumulator ( actorCount )
  accumulator.start ( )
  for ( index in 0 ..< actorCount ) {
    final localIndex = index
    Actors.actor { accumulator << ( new ProcessSlice ( localIndex , sliceSize , delta ) ).compute ( ) }
  }
  accumulator.join ( )
  final double pi = 4.0d * accumulator.sum * delta
  final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  println ( "==== Groovy/Java GPars DynamicDispatchActorScript pi = " + pi )
  println ( "==== Groovy/Java GPars DynamicDispatchActorScript iteration count = " + n )
  println ( "==== Groovy/Java GPars DynamicDispatchActorScript elapse = " + elapseTime )
  println ( "==== Groovy/Java GPars DynamicDispatchActorScript processor count = " + Runtime.runtime.availableProcessors ( ) )
  println ( "==== Groovy/Java GPars DynamicDispatchActorScript actor count = " + actorCount )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
