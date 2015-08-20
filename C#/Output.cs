/*
 *  Output routines for calculation of π using quadrature.
 *
 *  Copyright © 2015  Russel Winder
 */

public class Output {

  public static void output(string name, double pi, long iterations, double elapseTime, string additional = "") {
    System.Console.WriteLine("========================== " + name + "\n" +
                             "\tπ = " + pi + "\n" +
                             "\titerations = " + iterations + "\n" +
                             "\telapse time = " + elapseTime +
                             additional);
  }

  public static void output(string name, double pi, long iterations, double elapseTime, int threadCount) {
    output(
           name + " (thread count = " + threadCount + ")",
           pi,
           iterations,
           elapseTime,
           "\n\tnumber of processors = " + System.Environment.ProcessorCount);
  }

}
