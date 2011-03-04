/*
 *  Calculation of Pi using quadrature realized with GPars data flow tasks and an agent.
 *
 *  Copyright © 2010--2011 Russel Winder
 */

/*
 *  This program provided by Václav Pech by private email.  It is based on
 *  Pi_Java_ThreadsSynchronizedStatement amended to use GPars data flow variables and agents.
 */

import groovyx.gpars.MessagingRunnable ;
import groovyx.gpars.agent.Agent ;
import groovyx.gpars.dataflow.DataFlow ;
import groovyx.gpars.dataflow.DataFlowVariable ;

public class Pi_Java_GPars_DataFlowAndAgent {
  private static class Accumulator {
    private double sum = 0.0 ;
    void add ( final double value ) { sum += value ; }
    public double getSum ( ) { return sum ; }
  }
  private static Agent<Accumulator> sum ;
  private static void execute ( final int numberOfTasks ) throws InterruptedException {
    final int n = 1000000000 ;
    final double delta = 1.0 / n ;
    final long startTimeNanos = System.nanoTime ( ) ;
    final int sliceSize = n / numberOfTasks ;
    final DataFlowVariable<?>[] tasks = new DataFlowVariable[numberOfTasks] ;
    sum = new Agent<Accumulator> ( new Accumulator ( ) ) ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final int taskId = i ;
      tasks[taskId] = DataFlow.task ( new Runnable ( ) {
          @Override public void run ( ) {
            final int start = 1 + taskId * sliceSize ;
            final int end = (taskId + 1) * sliceSize ;
            double localSum = 0.0 ;
            for ( int i = start ; i <= end ; ++i ) {
              final double x = ( i - 0.5d ) * delta ;
              localSum += 1.0 / ( 1.0 + x * x ) ;
            }
            final double currentSum = localSum ;
            sum.send ( new MessagingRunnable<Accumulator> ( ) {
                @Override protected void doRun ( final Accumulator t ) {
                  t.add ( currentSum ) ;
                }
              } ) ;
          }
        } ) ;
    }
    for ( final DataFlowVariable<?> t : tasks ) {
      try { t.join ( ) ; }
      catch ( final InterruptedException ie ) { throw new RuntimeException ( "Got an InterruptedException joining a thread." , ie ) ; }
    }
    final double pi = 4.0 * sum.getVal ( ).getSum ( ) * delta;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    System.out.println ( "==== Java GPars Dataflow/Agent pi = " + pi ) ;
    System.out.println ( "==== Java GPars Dataflow/Agent iteration count = " + n ) ;
    System.out.println ( "==== Java GPars Dataflow/Agent elapse = " + elapseTime ) ;
    System.out.println ( "==== Java GPars Dataflow/Agent processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "==== Java GPars Dataflow/Agent thread count = " + numberOfTasks ) ;
  }
  public static void main ( final String[] args) throws InterruptedException {
    Pi_Java_GPars_DataFlowAndAgent.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_GPars_DataFlowAndAgent.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_GPars_DataFlowAndAgent.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_GPars_DataFlowAndAgent.execute ( 32 ) ;
  }
}
