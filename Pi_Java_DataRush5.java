/*
 *  Calculation of Pi using quadrature realized with a dataflow architecture implemented using Pervasive
 *  DataRush 5.
 *
 *  Copyright © 2009--2011 Russel Winder
 *  Copyright © 2009 Pervasive Software Inc.
 */

import com.pervasive.datarush.flows.ScalarFlow ;
import com.pervasive.datarush.graphs.ApplicationGraph;
import com.pervasive.datarush.operators.CompositionContext ;
import com.pervasive.datarush.operators.DataflowOperator ;
import com.pervasive.datarush.operators.DataflowProcess ;
import com.pervasive.datarush.operators.GraphFactory ;
import com.pervasive.datarush.ports.DoubleInput ;
import com.pervasive.datarush.ports.DoubleOutput ;

/**
 *  The original DataRush 4 version was created by Matt Walker (of Pervasive Software) based on the
 *  Pi_Java_Sequential.java and Pi_Java_Futures.java written by Russel Winder.  Russel Winder then added
 *  various bits and pieces and reformatted the DataRush 4 to be in a style consistent with the various
 *  other Java versions, not to mention later giving it a restructuring to get rid of the global variable --
 *  even though it wasn't shared.  Russel Winder then did the port from DataRush 4 to DataRush 5 when that
 *  was released 2011-02-02.
 *
 *  @author Russel Winder
 *  @author Matt Walker
 */
public class Pi_Java_DataRush5 {
  private static final int n = 1000000000 ;
  private static final double delta = 1.0 / n ;
  private static final class ComputeProcess extends DataflowProcess {
    private final int taskId ;
    private final int sliceSize ;
    private final DoubleOutput output ;
    public ComputeProcess ( final CompositionContext context , final int taskId , final int sliceSize ) {
      super ( context ) ;
      this.taskId = taskId ;
      this.sliceSize = sliceSize ;
      output = newDoubleOutput ( "output" ) ;
    }
    public ScalarFlow getOutput ( ) { return getFlow ( output ) ; }    
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
  private static final class AccumulatorProcess extends DataflowProcess {
    private final DoubleInput[] inputs ;
    private final long startTimeNanos ;
    public AccumulatorProcess ( final CompositionContext context , final ScalarFlow[] flows , final long startTimeNanos ) {
      super ( context ) ;
      inputs = new DoubleInput[flows.length] ;
      for ( int i = 0 ; i < flows.length ; ++i ) { inputs[i] = newDoubleInput ( flows[i] , "input" ) ; }
      this.startTimeNanos = startTimeNanos ;
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
      System.out.println("==== Java DataRush5 pi = " + pi ) ;
      System.out.println("==== Java DataRush5 iteration count = " + n ) ;
      System.out.println("==== Java DataRush5 elapse = " + elapseTime ) ;
      System.out.println("==== Java DataRush5 processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
      System.out.println("==== Java DataRush5 task count = " + ( inputs.length + 1 ) ) ;
    }
  }
  private static final class PiOperator extends DataflowOperator {
    private final int sliceSize ;
    private final int numberOfTasks ;
    private final long startTimeNanos ;
    public PiOperator ( final int numberOfTasks , final long startTimeNanos ) {
      this.sliceSize = n / numberOfTasks ;
      this.numberOfTasks = numberOfTasks ;
      this.startTimeNanos = startTimeNanos ;
    }
    @Override public void compose ( final CompositionContext context ) {
      final ScalarFlow[] results = new ScalarFlow[numberOfTasks] ;
      for ( int i = 0 ; i < numberOfTasks ; ++i ) {
        final ComputeProcess computeProcess = context.add ( new ComputeProcess ( context , i , sliceSize ) , "task" + i ) ;
        results[i] = computeProcess.getOutput ( ) ;
      }
      context.add ( new AccumulatorProcess ( context , results , startTimeNanos ) , "sum" ) ;
    }
  }
  private static void execute ( final int numberOfTasks ) {
    final long startTimeNanos = System.nanoTime ( ) ;
    final ApplicationGraph applicationGraph = GraphFactory.newApplicationGraph ( "pi" ) ;
    applicationGraph.add ( new PiOperator ( numberOfTasks , startTimeNanos ) ) ;
    applicationGraph.run ( ) ;
  }
  public static void main ( final String[] args ) {
    Pi_Java_DataRush5.execute ( 1 ) ;
    System.out.println ( ) ;
    Pi_Java_DataRush5.execute ( 2 ) ;
    System.out.println ( ) ;
    Pi_Java_DataRush5.execute ( 8 ) ;
    System.out.println ( ) ;
    Pi_Java_DataRush5.execute ( 32 ) ;
  }
}
