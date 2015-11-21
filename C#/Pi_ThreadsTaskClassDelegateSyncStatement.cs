/*
 *  Calculation of π using quadrature realized with a fork/join approach with threads.
 *
 *  Copyright © 2009, 2011, 2015  Russel Winder
 */

public class Pi_CS_ThreadsTaskClassDelegateSyncStatement {

  private static double sum = 0.0;
  private static object sumMutex = new object();

  private class Task {
    private readonly int start;
    private readonly int end;
    private readonly double delta;
    public Task(int start , int end , double delta) {
      this.start = start;
      this.end = end;
      this.delta = delta;
    }

    public void execute() {
      double localSum = 0.0;
      for (int i = start; i <= end; ++i) {
        double x = (i - 0.5) * delta;
        localSum += 1.0 / (1.0 + x * x);
      }
      lock (sumMutex) { sum += localSum; }
    }
  }

  private static void execute(int numberOfTasks) {
    const int n = 1000000000;
    const double delta = 1.0 / n;
    long startTimeHundredsOfNanos = System.DateTime.Now.Ticks;
    int sliceSize = n / numberOfTasks;
    System.Threading.Thread[] threads = new System.Threading.Thread[numberOfTasks];
    for (int i = 0; i < numberOfTasks; ++i) {
      Task task = new Task(1 + i * sliceSize ,  (i + 1) * sliceSize , delta);
      threads[i] = new System.Threading.Thread(new System.Threading.ThreadStart(task.execute));
    }
    foreach (System.Threading.Thread t in threads) { t.Start(); }
    foreach (System.Threading.Thread t in threads) { t.Join(); }
    double pi = 4.0 * delta * sum;
    double elapseTime = (System.DateTime.Now.Ticks - startTimeHundredsOfNanos) / 1e7;
    Output.output("Threads Task Class Delegate Sync Statement", pi, n, elapseTime, numberOfTasks);
  }

  public static void Main(string[] args) {
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute(1);
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute(2);
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute(8);
    Pi_CS_ThreadsTaskClassDelegateSyncStatement.execute(32);
  }

}
