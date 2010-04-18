/*
 *  Support class for the Java/Groovy version of the computation.
 *
 *  Copyright © 2010 Russel Winder
 */

public class ProcessSlice {
  private final int taskId ;
  private final long sliceSize ;
  private final double delta ;
  public ProcessSlice ( final int taskId , final long sliceSize , final double delta ) {
    this.taskId = taskId ;
    this.sliceSize = sliceSize ;
    this.delta = delta ;
  }  
  public double compute ( ) {
    final long start = 1 + taskId * sliceSize ;
    final long end = ( taskId + 1 ) * sliceSize ;
    double sum = 0.0 ;
    for ( long j = start ; j <= end ; ++j ) {
      final double x = ( j - 0.5d ) * delta ;
      sum += 1.0d / ( 1.0d + x * x ) ;
    }
    return sum ;
  }
}
