/*
 *  Calculation of Pi using quadrature realized with a dataflow architecture implemented by Pervasive
 *  DataRush 4.
 *
 *  Copyright (c) 2009--2011 Russel Winder
 *  Copyright (c) 2009 Pervasive Software Inc.
 */

import com.pervasive.datarush.flows.DoubleFlow ;
import com.pervasive.datarush.operators.ApplicationGraph;
import com.pervasive.datarush.operators.DataflowGraphBase ;
import com.pervasive.datarush.operators.DataflowNodeBase ;
import com.pervasive.datarush.operators.OperatorFactory ;
import com.pervasive.datarush.operators.PortArrayOperatorsFactory ;
import com.pervasive.datarush.ports.DoubleInput ;
import com.pervasive.datarush.ports.DoubleOutput ;

/**
 *  The original version of this DataRush version was written by Matt Walker (of Pervasive Software) based
 *  on the Pi_Java_sequential.java and Pi_Java_futures.java written by Russel Winder.  Russel Winder then
 *  added various bits and pieces and reformatted it to be in a style consistent with the other Java versions,
 *  not to mention later giving it a restructuring to get rid of the global variable -- even though it
 *  wasn't shared.
 *
 *  @author Russel Winder
 *  @author Matt Walker
 */
public class Pi_Java_DataRush4 {
  private static final int n = 1000000000 ;
  private static final double delta = 1.0 / n ;
  private static final class Task extends DataflowNodeBase {
    private final int taskId ;
    private final int sliceSize ;
    private final DoubleOutput output ;
    public Task ( final int taskId , final int sliceSize ) {
      this.taskId = taskId ;
      this.sliceSize = sliceSize ;
      output = newDoubleOutput ( "output" ) ;
    }
    @Override public void execute ( ) {
      final int start = 1 + taskId * sliceSize ;
      final int end = ( taskId + 1 ) * sliceSize ;
      double sum = 0.0 ;
      for ( int i = start ; i <= end ; ++i ) {
        final double x = ( i - 0.5 ) * delta ;
        sum += 1.0 / ( 1.0 + x * x ) ;
      }
      output.push ( sum ) ;
      output.pushEndOfData ( ) ;
    }
  }
  private static final class Accumulator extends DataflowNodeBase {
    private final DoubleInput[] inputs ;
    private final long startTimeNanos ;
    public Accumulator ( final DoubleFlow[] flows , final long startTimeNanos ) {
      this.startTimeNanos = startTimeNanos ;
      inputs = new PortArrayOperatorsFactory ( this ).newDoubleInputs ( "input" , flows ) ;
    }
    @Override public void execute ( ) {
      double sum = 0.0 ;
      for ( final DoubleInput input : inputs ) {
        input.stepNext ( ) ;
        sum += input.asDouble ( ) ;
        input.stepNext ( ) ; // Should be at EOD
      }
      final double pi = 4.0 * sum * delta ;
      final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
      System.out.println ( "==== Java DataRush4 pi = " + pi ) ;
      System.out.println ( "==== Java DataRush4 iteration count = " + n ) ;
      System.out.println ( "==== Java DataRush4 elapse = " + elapseTime ) ;
      System.out.println ( "==== Java DataRush4 processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
      System.out.println ( "==== Java DataRush4 task count = " + ( inputs.length + 1 ) ) ;
    }
  }
  private static final class PiGraphNode extends DataflowGraphBase {
    public PiGraphNode ( final int numberOfTasks , final long startTimeNanos ) {
      final int sliceSize = n / numberOfTasks ;
      final DoubleFlow[] results = new DoubleFlow[numberOfTasks] ;
      for ( int i = 0 ; i < numberOfTasks ; ++i ) {
        final Task task = add ( new Task ( i , sliceSize) , "task " + i ) ;
        results[i] = task.output.getFlow ( ) ;
      }
      add ( new Accumulator ( results , startTimeNanos ) , "sum" ) ;
    }
  }
  private static void execute ( final int numberOfTasks ) {
    final long startTimeNanos = System.nanoTime ( ) ;
    final ApplicationGraph applicationGraph = OperatorFactory.newApplicationGraph ( "pi" ) ;
    applicationGraph.add ( new PiGraphNode ( numberOfTasks , startTimeNanos ) , "pi" ) ;
    applicationGraph.run ( ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_DataRush4.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_DataRush4.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_DataRush4.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_DataRush4.execute ( 32 ) ;
  }
}
