#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with GPars actors. Scripty.  With Java computation.
 *
 *  Copyright © 2009–2012 Russel Winder.
 */

import groovyx.gpars.actor.Actor
import groovyx.gpars.group.DefaultPGroup

void execute ( final int actorCount ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  final int sliceSize = n / actorCount
  final group = new DefaultPGroup ( actorCount + 1i )
  final accumulator = group.messageHandler {
    double sum = 0.0d
    int count = 0i
    when { double result ->
      sum +=  result
      if ( ++count == actorCount ) {
        final double pi = 4.0d * delta * sum
        final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
        Output.out ( 'Groovy/Java GPars ActorScript' , pi , n , elapseTime , actorCount )
        terminate ( )
      }
    }
  }
  final computors = [ ]  
  //  Loop variables are not captured at definition time but at execution time so use the trick used in Java
  //  to ensure correct capture of the index number for the slice. It would be much better if Groovy had
  //  some form of list comprehension capability.
  for ( i in 0 ..< actorCount ) {
    final int index = i
    computors.add (
      group.actor {
        accumulator << ( new ProcessSlice ( index , sliceSize , delta ) ).compute ( )
      }
    )
  }
  accumulator.join ( )
}

execute ( 1 )
execute ( 2 )
execute ( 8 )
execute ( 32 )
