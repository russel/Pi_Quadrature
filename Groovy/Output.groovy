/**
 *  Output functions for the Groovy codes.
 *
 *  Copyright © 2012 Russel Winder
 */

static void out(prefix, pi, n, elapseTime) {
  println('================ ' + prefix)
  println('\tπ = ' + pi)
  println('\titeration count = ' + n)
  println('\telapse time = ' + elapseTime)
}

static void out(prefix, pi, n, elapseTime, numberOfTasks) {
  out(prefix, pi, n, elapseTime)
  println('\tprocessor count = ' + Runtime.runtime.availableProcessors())
  println('\tnumber of things = ' + numberOfTasks)
}
