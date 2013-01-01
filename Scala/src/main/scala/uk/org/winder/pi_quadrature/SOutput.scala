/*
 *  Output function for all the Scala codes.
 *
 *  Copyright © 2012 Russel Winder
 */

object SOutput {
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
