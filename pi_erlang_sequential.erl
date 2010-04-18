%  Calculation of Pi using quadrature. Sequential algorithm.
%
%  Copyright © 2008-9 Russel Winder

-module ( pi_erlang_sequential ) .
-export ( [ start / 0 ] ) .
-import ( microsecondTime , [ microsecondTime / 0 ] ) .

%%%  The obvious algorithm uses some form of list either explicitly as below or via the use of map or
%%%  mapfoldr.  This list-based approach cannot work for the huge numbers of iterations we have here as
%%%  there is just not enough memory.  The only way out of this is to use tail recursion to do iteration.

%%pi ( N ) ->
%%    Delta = 1 / N ,
%%    Xs = [ ( I - 0.5 ) * Delta || I <- lists:seq ( 1 , N ) ] ,
%%    lists:sum ( [ 1.0 / ( 1.0 + X * X ) || X <- Xs ] ) * 4.0 * Delta .

pi_iter ( 0 , Delta , Sum ) -> 4.0 * Delta * Sum ;
pi_iter ( I , Delta , Sum ) ->
    X = ( I - 0.5 ) * Delta ,
    pi_iter ( I - 1 , Delta , Sum + 1.0 / ( 1.0 + X * X ) ) .

pi ( N ) -> pi_iter ( N , 1 / N , 0.0 ) .

start ( ) ->
    N = 100000000 , % 10 times fewer due to speed issues.
    StartTime = microsecondTime ( ) ,
    Pi = pi ( N ) ,
    ElapseTime = microsecondTime ( ) - StartTime ,
    io:format ( "==== Erlang Sequential pi = ~p~n" , [ Pi ] ) ,
    io:format ( "==== Erlang Sequential iteration count = ~p~n" , [ N ] ) ,
    io:format ( "==== Erlang Sequential elapse = ~p~n" , [ ElapseTime ] ) ,
    halt ( ) .
