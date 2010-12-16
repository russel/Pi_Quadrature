#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.  With Java computation.
 *
 *  Copyright © 2009-10 Russel Winder.
 */

@Grab ( 'org.codehaus.gpars:gpars:0.11-beta-4' )

import groovyx.gpars.actor.Actor
import groovyx.gpars.group.DefaultPGroup

void execute ( final int actorCount ) {
  final long n = 1000000000l
  final double delta = 1.0d / n
  final long sliceSize = n / actorCount
  final long startTimeNanos = System.nanoTime ( )
  final computors = [ ]
  final group = new DefaultPGroup ( actorCount + 1 )
  final accumulator = group.messageHandler {
    double sum = 0.0d
    int count = 0
    when { double result ->
      sum +=  result
      if ( ++count == actorCount ) {
        final double pi = 4.0d * sum * delta
        final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
        println ( '==== Groovy/Java GPars ActorScript pi = ' + pi )
        println ( '==== Groovy/Java GPars ActorScript iteration count = ' + n )
        println ( '==== Groovy/Java GPars ActorScript elapse = ' + elapseTime )
        println ( '==== Groovy/Java GPars ActorScript processor count = ' + Runtime.runtime.availableProcessors ( ) ) ;
        println ( '==== Groovy/Java GPars ActorScript actor count = ' + actorCount )
        terminate ( )
      }
    }
  }
  for ( index in 0..< actorCount ) {
    computors.add (
      group.actor {
        accumulator << ( new ProcessSlice ( index , sliceSize , delta ) ).compute ( )
      }
    )    
  }
  accumulator.join ( )
}

execute ( 1 )
println ( )
execute ( 2 )
println ( )
execute ( 8 )
println ( )
execute ( 32 )
