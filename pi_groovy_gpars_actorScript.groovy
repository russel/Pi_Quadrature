#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.
 *
 *  Copyright © 2009-10 Russel Winder.
 */

@Grab ( group = 'org.codehaus.gpars' ,  module = 'gpars' , version = '0.10-beta-1-SNAPSHOT' )

import groovyx.gpars.actor.PooledActorGroup
import groovyx.gpars.actor.impl.RunnableBackedPooledActor

void execute ( final int actorCount ) {
  final long n = 100000000l // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final long sliceSize = n / actorCount
  final long startTimeNanos = System.nanoTime ( )
  final computors = [ ]
  final group = new PooledActorGroup ( actorCount + 1 )
  //final accumulator = group.actor {  //  <--- Don't use this sort of thing here it gives weird results!!!!!!
  final accumulator = new RunnableBackedPooledActor ( ) {
    @Override protected void act ( ) {
      double sum = 0.0d
      for ( c in computors ) { receive { sum +=  it } }
      final double pi = 4.0d * sum * delta
      final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
      println ( "==== Groovy GPars ActorScript pi = " + pi )
      println ( "==== Groovy GPars ActorScript iteration count = " + n )
      println ( "==== Groovy GPars ActorScript elapse = " + elapseTime )
      println ( "==== Groovy GPars ActorScript processor count = " + Runtime.runtime.availableProcessors ( ) ) ;
      println ( "==== Groovy GPars ActorScript actor count = " + actorCount )
    }
  }
  for ( index in 0l..< actorCount ) {
    final long start = 1l + index * sliceSize
    final long end = ( index + 1l ) * sliceSize
    computors.add (
      group.actor {
        double sum = 0.0d
        for ( long i = start ; i <= end ; ++i ) {
          final double x = ( i - 0.5d ) * delta
          sum += 1.0d / ( 1.0d + x * x )
        }
        accumulator << sum
      }
    )    
  }
  accumulator.start ( )
  accumulator.join ( )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
