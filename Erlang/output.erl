-module(output).
-export([out/4, out/5]).

out(Name, Pi, N, ElapseTime) ->
    io:format("==================== ~s~n", [Name]),
    io:format("    π = ~p~n", [Pi]),
    io:format("    iteration count = ~p~n", [N]),
    io:format("    elapse time = ~p~n", [ElapseTime]).

out(Name, Pi, N, ElapseTime, NWorkers) ->
    out(Name ++ ": task count " ++ integer_to_list(NWorkers), Pi, N, ElapseTime),
    io:format("    scheduler count = ~p~n", [erlang:system_info(schedulers)]).
