#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.
 *
 *  Copyright © 2009–2011 Russel Winder.
 */

@Grab ( 'org.codehaus.gpars:gpars:1.0-SNAPSHOT' )

import groovyx.gpars.actor.Actor
import groovyx.gpars.group.DefaultPGroup

void execute ( final int actorCount ) {
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / actorCount
  final group = new DefaultPGroup ( actorCount + 1i )
  final accumulator = group.messageHandler {
    double sum = 0.0d
    int count = 0i
    when { double result ->
      sum += result
      if ( ++count == actorCount ) {
        final double pi = 4.0d * delta * sum
        final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
        println ( '==== Groovy GPars ActorScript pi = ' + pi )
        println ( '==== Groovy GPars ActorScript iteration count = ' + n )
        println ( '==== Groovy GPars ActorScript elapse = ' + elapseTime )
        println ( '==== Groovy GPars ActorScript processor count = ' + Runtime.runtime.availableProcessors ( ) ) ;
        println ( '==== Groovy GPars ActorScript actor count = ' + actorCount )
        terminate ( )
      }
    }
  }
  final computors = [ ]
  //  Loop variables are not captured at definition time but at execution time so use the trick used in Java
  //  to ensure correct capture of the index number for the slice.
  for ( a in 0 ..< actorCount ) {
    final int index = a
    computors.add (
      group.actor {
        final int start = 1i + index * sliceSize
        final int end = ( index + 1i ) * sliceSize
        double sum = 0.0d
        for ( int i in start .. end ) {
          final double x = ( i - 0.5d ) * delta
          sum += 1.0d / ( 1.0d + x * x )
        }
        accumulator << sum
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
