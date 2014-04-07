/**
 *  Output functions for the Groovy codes.
 *
 *  Copyright © 2012–2014  Russel Winder
 */

static void out(final String prefix, final double pi, final int n, final double elapseTime) {
  println "==================== ${prefix}"
  println "\tπ = ${pi}"
  println "\titeration count = ${n}"
  println "\telapse time = ${elapseTime}"
}

static void out(final Class klass, final double pi, final int n, final double elapseTime) {
  out klass.name, pi, n, elapseTime
}

static void out(final String prefix, final double pi, final int n, final double elapseTime, final int numberOfTasks) {
  out "${prefix}; number of tasks = ${numberOfTasks.toString()}", pi, n, elapseTime
  println "\tprocessor count = ${Runtime.runtime.availableProcessors()}"
}

static void out(final Class klass, final double pi, final int n, final double elapseTime, final int numberOfTasks) {
  out klass.name, pi, n, elapseTime, numberOfTasks
}
