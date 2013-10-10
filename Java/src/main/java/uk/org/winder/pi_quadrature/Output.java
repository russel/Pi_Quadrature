/*
 *  Output functions for π using quadrature codes.
 *
 *  Copyright © 2009–2013  Russel Winder
 */

package uk.org.winder.pi_quadrature;

public class Output {

  public static void out(final String prefix, final Double pi, final Integer n, final Double elapseTime) {
    System.out.println("====================== " + prefix);
    System.out.println("\tπ = " + pi);
    System.out.println("\titeration count = " + n);
    System.out.println("\telapse time = " + elapseTime);
  }

  public static void out(final String prefix, final Double pi, final Integer n, final Double elapseTime, final Integer numberOfTasks) {
    out(prefix + ": task count = " + numberOfTasks.toString(), pi, n, elapseTime);
    System.out.println("\tprocessor count = " + Runtime.getRuntime().availableProcessors());
  }

}
