%  Calculation of Pi using quadrature.  Using Joe Armstrong's pmap.
%
%  Copyright © 2009 Russel Winder

-module ( pi_erlang_pmap ) .
-export ( [ start / 0 ] ) .
-import ( microsecondTime , [ microsecondTime / 0 ] ) .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is Joe Armstrong's implementation of pmap taken from "Programming Erlang: Software for a Concurrent
% World", p366.  See also: http://www.erlang.org/ml-archive/erlang-questions/200606/msg00187.html and
% http://erlang.org/pipermail/erlang-questions/2006-June/020832.html

pmap ( F , L ) -> 
    S = self ( ) ,
    Ref = erlang:make_ref ( ) ,
    Pids = lists:map ( fun ( I ) -> spawn ( fun ( ) -> do_f ( S , Ref , F , I ) end ) end , L ) ,
    gather ( Pids , Ref ) .

do_f ( Parent , Ref , F , I ) -> Parent ! { self ( ) , Ref , ( catch F ( I ) ) } .

gather ( [ Pid | T ] , Ref ) -> receive { Pid , Ref , Ret } -> [ Ret | gather ( T , Ref ) ] end ;
gather ( [ ] , _ ) -> [ ] .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi_iter ( _To , _To , Delta , Sum ) -> 4.0 * Sum * Delta ;
pi_iter (  I , To , Delta , Sum ) ->
	X = ( I - 0.5 ) * Delta ,
	pi_iter ( I + 1 , To , Delta , Sum + 1.0 / ( 1.0 + X * X ) ) .

pi ( N , NWorkers ) ->
    NPerWorker = trunc ( N / NWorkers + 0.5 ) ,
    Delta = 1 / N ,
    lists:sum ( pmap ( fun ( { From , To } ) -> pi_iter ( From , To , Delta , 0.0 ) end , [ { I , lists:min ( [ N , I + NPerWorker - 1 ] ) }  || I <- lists:seq ( 1 , N , NPerWorker ) ] ) ) .

execute ( NWorkers ) ->
    N = 100000000 , % 10 times fewer due to speed issues.
    StartTime = microsecondTime ( ) ,
    Pi = pi ( N , NWorkers ) ,
    ElapseTime = microsecondTime ( ) - StartTime ,
    io:format ( "==== Erlang PMap pi = ~p~n" , [ Pi ] ) ,
    io:format ( "==== Erlang PMap iteration count = ~p~n" , [ N ] ) ,
    io:format ( "==== Erlang PMap elapse = ~p~n" , [ ElapseTime ] ) ,
    io:format ( "==== Erlang PMap scheduler count = ~p~n" , [ erlang:system_info ( schedulers ) ] ) ,
    io:format ( "==== Erlang PMap worker count = ~p~n" , [ NWorkers ] ) .

start ( ) ->
    execute ( 1 ) ,
    io:format ( "~n" ) ,
    execute ( 2 ) ,
    io:format ( "~n" ) ,
    execute ( 8 ) ,
    io:format ( "~n" ) ,
    execute ( 32 ) ,
    halt ( ) .
