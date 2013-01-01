/*
 *  Output function for all the Scala codes.
 *
 *  Copyright © 2012–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature

object Output {

  def out(banner:String, pi:Double, n:Int, elapseTime:Double) {
    println("================ " + banner)
    println("\tπ = " + pi)
    println("\titeration count = " + n)
    println("\telapse time = " + elapseTime)
  }

  def out(banner:String, pi:Double, n:Int, elapseTime:Double, numberOfTasks:Int) {
    out(banner, pi, n, elapseTime)
    println("\tprocessor count = " + Runtime.getRuntime.availableProcessors)
    println("\tnumber of tasks = " + numberOfTasks)
  }

}
