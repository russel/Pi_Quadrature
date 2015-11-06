:- module(output, [out/4]).

out(Banner, Pi, N, T_elapse) :-
    format('==================== ~s~n', [Banner]),
    format('~sπ = ~18f~n', ['\t', Pi]),
    format('~siteration count = ~d~n', ['\t', N]),
    format('~selapse time = ~f~n', ['\t', T_elapse]).
