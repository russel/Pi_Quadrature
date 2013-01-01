import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.Exit
import scala.actors.remote.RemoteActor._
import scala.actors.remote.Node


/** This file is the same as PiByQuadrature but with Remote rather
 * than local Actors.
 */

class RemoteCalculator(id : Int, sliceSize : Double, delta : Double, acc : Actor) extends Actor {
	alive(9898)
	register('Calculator, self)
	
	def act() {
		var sum : Double = 0.0
		var x : Double = 0.0
		for (i <- 1 + id * sliceSize.toInt to (id + 1) * sliceSize.toInt + 1) {
			x = ( i - 0.5 ) * delta
			sum += 1.0 / ( 1.0 + x * x )
		} 
		acc ! sum
	}
}


class RemoteAccumulator(numberOfActors : Int, delta : Double) extends Actor {
    alive(9898)
    register('Accumulator, self)	
    var sum : Double = 0.0
    def act {
        Actor.receive {
            case i : Int =>
            	for (i <- 0 until numberOfActors) {
        	    Actor.receive {
        		    case d : Double => 
	    		        sum += d.asInstanceOf[Double] 
	      	    }
		}
	        reply(4.0 * sum * delta)
	}
    }
}

object Pi_Scala_RemoteActors_SM {
	
	def calculatePi(numberOfActors: Int) : Double = {
		val n : Double = 1000000000.0
		val delta : Double = 1.0 / n
		val sliceSize : Double = n / numberOfActors

		val accumulator = new RemoteAccumulator(numberOfActors, delta)
		accumulator.start()
		accumulator ! 0

		val calculators : Array[Actor] = new Array[Actor](numberOfActors)			
		for (i <- calculators.indices) {
			calculators(i) = new RemoteCalculator(i, sliceSize, delta, accumulator)
			calculators(i).start()
		}

        	/* Receive the final value of pi from the accumulator Actor. */
        	Actor.receive { case pi : Double => return pi }
	}
	
	def main(args : Array[String]) : Unit = {
		val timer = new Timer[Int, Double]
		var pi : Double = 0.0
		var numberOfActors : Int = 0
		for (powOfTwo <- 0 to 10) {
		    numberOfActors = (scala.math.pow(2, powOfTwo)).toInt
    		println("-------------------------------------------------------------")
    		println("Number of Actors used: " + numberOfActors)
    		pi = timer.time(calculatePi, numberOfActors, 10)
	    	println("pi = " + pi)
	    }
	}
	
}
