/*
 *  Support class for the Java/Groovy version of the computation using JCSP.
 *
 *  Copyright © 2010 Russel Winder
 */

import org.jcsp.lang.ChannelOutput ;
import org.jcsp.lang.CSProcess ;

public class ProcessSlice_JCSP implements CSProcess {
  private final int taskId ;
  private final long sliceSize ;
  private final double delta ;
  private final ChannelOutput<Double> out ;
  public ProcessSlice_JCSP ( final int taskId , final long sliceSize , final double delta , final ChannelOutput<Double> out ) {
    this.taskId = taskId ;
    this.sliceSize = sliceSize ;
    this.delta = delta ;
    this.out = out ;
  }  
  public void run ( ) {
    final long start = 1 + taskId * sliceSize ;
    final long end = ( taskId + 1 ) * sliceSize ;
    double sum = 0.0 ;
    for ( long j = start ; j <= end ; ++j ) {
      final double x = ( j - 0.5d ) * delta ;
      sum += 1.0d / ( 1.0d + x * x ) ;
    }
    out.write ( sum ) ;
  }
}
