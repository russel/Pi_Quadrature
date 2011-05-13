#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.  With Java computation.
 *
 *  Copyright © 2009--2011 Russel Winder.
 */

@Grab ( 'org.codehaus.gpars:gpars:0.12-beta-1' )

import groovyx.gpars.actor.Actor
import groovyx.gpars.group.DefaultPGroup

void execute ( final int actorCount ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / actorCount
  final computors = [ ]
  final group = new DefaultPGroup ( actorCount + 1i )
  final accumulator = group.messageHandler {
    double sum = 0.0d
    int count = 0i
    when { double result ->
      sum +=  result
      if ( ++count == actorCount ) {
        final double pi = 4.0d * sum * delta
        final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
        println ( '==== Groovy/Java GPars ActorScript pi = ' + pi )
        println ( '==== Groovy/Java GPars ActorScript iteration count = ' + n )
        println ( '==== Groovy/Java GPars ActorScript elapse = ' + elapseTime )
        println ( '==== Groovy/Java GPars ActorScript processor count = ' + Runtime.runtime.availableProcessors ( ) ) ;
        println ( '==== Groovy/Java GPars ActorScript actor count = ' + actorCount )
        terminate ( )
      }
    }
  }
  
  println ( 'WWWW:' + sliceSize.class )
  for ( index in 0 ..< actorCount ) {
    println ( 'XXXX:' + sliceSize.class )
    computors.add (
      group.actor {
        println ( 'YYYY:' + sliceSize.class )
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
