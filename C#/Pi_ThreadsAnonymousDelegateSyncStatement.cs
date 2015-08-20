/*
 *  Calculation of π using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009, 2011, 2015  Russel Winder
 */

using static Output;

public class Pi_CS_ThreadsAnonymousDelegateSyncStatement {

  private static double sum = 0.0;
  private static object sumMutex = new object();

  private static void execute(int numberOfTasks) {
    const long n = 1000000000L;
    const double delta = 1.0 / n;
    long startTimeHundredsOfNanos = System.DateTime.Now.Ticks;
    long sliceSize = n / numberOfTasks;
    System.Threading.Thread[] threads = new System.Threading.Thread [ numberOfTasks ];
    for (int i = 0; i < numberOfTasks; ++i) {
      long start = 1 + i * sliceSize;
      long end =  (i + 1) * sliceSize;
      threads[i] = new System.Threading.Thread(new System.Threading.ThreadStart(delegate() {
            double localSum = 0.0;
            for (long j = start; j <= end; ++j) {
              double x = (j - 0.5) * delta;
              localSum += 1.0 / (1.0 + x * x);
            }
            lock (sumMutex) { sum += localSum; }
          }
         ));
    }
    foreach (System.Threading.Thread t in threads) { t.Start(); }
    foreach (System.Threading.Thread t in threads) { t.Join(); }
    double pi = 4.0 * delta * sum;
    double elapseTime = (System.DateTime.Now.Ticks - startTimeHundredsOfNanos) / 1e7;
    Output.output("Threads Anonymous Delegate Sync Statement", pi, n, elapseTime, numberOfTasks);
  }

  public static void Main(string[] args) {
    Pi_CS_ThreadsAnonymousDelegateSyncStatement.execute(1);
    Pi_CS_ThreadsAnonymousDelegateSyncStatement.execute(2);
    Pi_CS_ThreadsAnonymousDelegateSyncStatement.execute(8);
    Pi_CS_ThreadsAnonymousDelegateSyncStatement.execute(32);
  }

}
