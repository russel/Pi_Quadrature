-module ( microsecondTime ) .
-export ( [ microsecondTime / 0 ] ) .

microsecondTime ( ) ->
    { Megaseconds , Seconds , Microseconds } = erlang:now ( ) ,
    Megaseconds * 1000000 + Seconds  + Microseconds / 1000000 .
