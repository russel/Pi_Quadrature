#! /usr/bin/env groovy

/*
 *  Calculation of π using quadrature realized with an GPars actor based algorithm.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

//  This code evolved from one provided by Václav Pech by private email which was based on one of the
//  pre-existing Groovy/Java examples.

import groovyx.gpars.actor.Actors
import groovyx.gpars.actor.DynamicDispatchActor

final class DDAAccumulator extends DynamicDispatchActor {
  private final actorCount
  def sum = 0.0
  private def counter = 0
  def DDAAccumulator ( actorCount ) { this.actorCount = actorCount }
  @Override void onMessage ( value ) {
    sum += value
    if ( ++counter == actorCount ) { terminate ( ) }
  }
}

void execute ( final actorCount ) {
  final n = 1000000000
  final delta = 1.0 / n
  final startTimeNanos = System.nanoTime ( )
  final sliceSize = ( int ) ( n / actorCount )
  final accumulator = new DDAAccumulator ( actorCount )
  accumulator.start ( )
  ( 0 ..< actorCount ).each { id ->
    Actors.actor { accumulator << ( new ProcessSlice ( id , sliceSize , delta ) ).compute ( ) }
  }
  accumulator.join ( )
  final pi = 4.0 * delta * accumulator.sum
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  Output.out ( getClass ( ).name , pi , n , elapseTime , actorCount )
}

execute 1
execute 2
execute 8
execute 32
