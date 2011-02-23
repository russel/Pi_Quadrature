/*
 *  Calculation of Pi using quadrature realized with a dataflow architecture implemented by Pervasive
 *  DataRush 4.
 *
 *  Copyright (c) 2009-10 Russel Winder
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
 *  added various bits and pieces and reformatted it to be in a style consistent with the other versions.
 *
 *  @author Russel Winder
 *  @author Matt Walker
 */
public class Pi_Java_DataRush4 extends DataflowGraphBase {
  private static final long n = 1000000000l;
  private static final double delta = 1.0 / n;
  private double sum = 0.0 ;
  private static final class Task extends DataflowNodeBase {
    private final long id ;
    private final long sliceSize ;
    private final double delta ;  
    private final DoubleOutput output ;
    public Task ( final long id , final long sliceSize , final double delta ) {
      this.id = id ;
      this.sliceSize = sliceSize ;
      this.delta = delta ;
      output = newDoubleOutput ( "output" ) ;
    }
    @Override public void execute ( ) {
      final long start = 1 + id * sliceSize ;
      final long end = ( id + 1 ) * sliceSize ;
      double sum = 0.0 ;
      for ( long i = start ; i <= end ; ++i ) {
        final double x = ( i - 0.5 ) * delta ;
        sum += 1.0 / ( 1.0 + x * x ) ;
      }
      output.push ( sum ) ;
      output.pushEndOfData ( ) ;
    }
  }
  private final class Accumulator extends DataflowNodeBase {
    private final DoubleInput[] inputs ;
    public Accumulator ( final DoubleFlow[] flows ) {
      inputs = new PortArrayOperatorsFactory ( this ).newDoubleInputs ( "input" , flows ) ;
    }
    @Override public void execute ( ) {
      for ( final DoubleInput input : inputs ) {
        input.stepNext ( ) ;
        sum += input.asDouble ( ) ;
        input.stepNext ( ) ; // Should be at EOD
      }
    }
  }
  public Pi_Java_DataRush4 ( final int numberOfTasks ) {
    final long sliceSize = n / numberOfTasks ;
    final DoubleFlow[] results = new DoubleFlow[numberOfTasks] ;
    for ( int i = 0 ; i < numberOfTasks ; ++i ) {
      final Task task = add ( new Task ( i , sliceSize , delta ) , "task" + i ) ;
      results[i] = task.output.getFlow ( ) ;
    }
    add ( new Accumulator ( results ) , "sum" ) ;
  }
  private static void execute ( final int numberOfTasks ) {
    final long startTimeNanos = System.nanoTime ( ) ;
    final ApplicationGraph applicationGraph = OperatorFactory.newApplicationGraph ( "pi" ) ;
    final Pi_Java_DataRush4 piDR = applicationGraph.add ( new Pi_Java_DataRush4 ( numberOfTasks ) , "piDataRush" ) ;
    applicationGraph.run ( ) ;
    final double pi = 4.0 * piDR.sum * delta ;
    final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9 ;
    System.out.println("==== Java DataRush4 pi = " + pi ) ;
    System.out.println("==== Java DataRush4 iteration count = " + n ) ;
    System.out.println("==== Java DataRush4 elapse = " + elapseTime ) ;
    System.out.println("==== Java DataRush4 processor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println("==== Java DataRush4 task count = " + ( numberOfTasks + 1 ) ) ;
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
