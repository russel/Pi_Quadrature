/*
 *  Calculation of Pi using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009,2011 Russel Winder
 */
public class Pi_CS_ThreadsTaskClassDelegateSyncStatement {
  private static double sum = 0.0 ;
  private static object sumMutex = new object ( ) ;
  private class Task {
    private readonly long start ;
    private readonly long end ;
    private readonly double delta ;
    public Task ( long start , long end , double delta ) {
      this.start = start ;
      this.end = end ;
      this.delta = delta ;
    }
    public void execute ( ) {
      double localSum = 0.0 ;
      for ( long i = start ; i <= end ; ++i ) {
        double x = ( i - 0.5 ) * delta ;
        localSum += 1.0 / ( 1.0 + x * x ) ;
      }
      lock ( sumMutex ) { sum += localSum ; }
    }
  }
  private static void execute ( int numberOfTasks ) {
    const long n = 1000000000L ;
    const double delta = 1.0 / n ;
    long startTimeHundredsOfNanos = System.DateTime.Now.Ticks ;
    long sliceSize = n / numberOfTasks ;
    System.Threading.Thread[] threads = new System.Threading.Thread [ numberOfTasks ] ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      Task task = new Task ( 1 + i * sliceSize ,  ( i + 1 ) * sliceSize , delta ) ;
      threads[i] = new System.Threading.Thread ( new System.Threading.ThreadStart ( task.execute ) ) ;
    }
    foreach ( System.Threading.Thread t in threads ) { t.Start ( ) ; }
    foreach ( System.Threading.Thread t in threads ) { t.Join ( ) ; }
    double pi = 4.0 * delta * sum ;
    double elapseTime = ( System.DateTime.Now.Ticks - startTimeHundredsOfNanos ) / 1e7 ;
    System.Console.WriteLine ( "==== C# Threads Task Class Delegate Sync Statement pi = " + pi ) ;
    System.Console.WriteLine ( "==== C# Threads Task Class Delegate Sync Statement iteration count = " + n ) ;
    System.Console.WriteLine ( "==== C# Threads Task Class Delegate Sync Statement elapse = " + elapseTime ) ;
    System.Console.WriteLine ( "==== C# Threads Task Class Delegate Sync Statement processor count = " + System.Environment.ProcessorCount ) ;
    System.Console.WriteLine ( "==== C# Threads Task Class Delegate Sync Statement thread count = " + numberOfTasks ) ;
  }
  public static void Main ( string[] args ) {
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute ( 1 ) ;
    System.Console.WriteLine ( ) ;
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute ( 2 ) ;
    System.Console.WriteLine ( ) ;
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute ( 8 ) ;
    System.Console.WriteLine ( ) ;
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute ( 32 ) ;
  }
}
