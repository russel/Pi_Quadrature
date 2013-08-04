/*
 * Output function for all the Fantom variants of calculating π using quadrature.
 *
 *  Copyright © 2013  Russel Winder <russel@winder.org.uk>
 */

using [java] java.lang::Runtime

class Output {
  static Void out(Str banner, Float pi, Int n , Float elapseTime) {
    echo("================================== " + banner)
    echo("\tπ = " + pi)
    echo("\titeration count = " + n)
    echo("\telapse = " + elapseTime)
  }

  static Void outN(Str banner, Float pi, Int n , Float elapseTime, Int numberOfTasks) {
    out(banner + ", number of tasks = " + numberOfTasks, pi, n, elapseTime)
    echo("\tnumber of processors = " + Runtime.getRuntime.availableProcessors)
  }
}
