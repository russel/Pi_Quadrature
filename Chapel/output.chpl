/*
 * A Chapel procedure for doing result output.
 *
 *  Copyright © 2012, 2013  Russel Winder
 */

module Output {
  proc output(banner:string, pi:real, n:int, elapseTime:real) {
    writeln("======================== " + banner );
    writeln("\tπ = " , pi );
    writeln("\titeration count = " , n ) ;
    writeln("\telapse time = " , elapseTime ) ;
  }

  proc output_more(banner:string, pi:real, n:int, elapseTime:real, numberOfTasks:int) {
    output(banner + ", task count: " + numberOfTasks, pi, n, elapseTime);
    writeln("\tnumber of processors = ", here.numCores);
  }
}
