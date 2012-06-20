public class JOutput {
  public static void out ( String prefix , Double pi , Integer n , Double elapseTime ) {
    System.out.println ( "================ " + prefix ) ;
    System.out.println ( "\tπ = " + pi ) ;
    System.out.println ( "\titeration count = " + n ) ;
    System.out.println ( "\telapse time = " + elapseTime ) ;
  }
  public static void out ( String prefix , Double pi , Integer n , Double elapseTime , Integer numberOfTasks ) {
    out ( prefix , pi , n , elapseTime ) ;
    System.out.println ( "\tprocessor count = " + Runtime.getRuntime ( ).availableProcessors ( ) ) ;
    System.out.println ( "\tnumber of tasks = " + numberOfTasks ) ;
  }
}
