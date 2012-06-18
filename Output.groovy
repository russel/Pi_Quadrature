
class Output {
  static void out ( prefix , pi , n , elapseTime ) {
    println ( '================ ' + prefix )
    println ( '\tπ = ' + pi )
    println ( '\titeration count = ' + n )
    println ( '\telapse time = ' + elapseTime )
  }
  static void out ( prefix , pi , n , elapseTime , numberOfTasks ) {
    println ( '================ ' + prefix )
    println ( '\tπ = ' + pi )
    println ( '\titeration count = ' + n )
    println ( '\telapse time = ' + elapseTime )
    println ( '\tprocessor count = ' + Runtime.runtime.availableProcessors ( ) )
    println ( '\tnumber of things = ' + numberOfTasks )
  }
}
