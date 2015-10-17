/*
 *  Output functions for calculation of  π using quadrature.
 *
 *  Copyright © 2009–2013, 2015  Russel Winder <russel@winder.org.uk>
 */

package uk.org.winder.pi_quadrature;

import x10.io.Console;

class Output {

  static def out(banner:String, pi:Double, n:Long, elapseTime:Double):void {
    Console.OUT.println("======================== X10 " + banner);
    Console.OUT.println("\tπ = " + pi);
    Console.OUT.println("\titeration count = " + n);
    Console.OUT.println("\telapse time = " + elapseTime);
  }

  static def out(banner:String, pi:Double, n:Long, elapseTime:Double, pCount:Long):void {
    out(banner + ": " + pCount.toString(), pi, n, elapseTime);
  }

}
